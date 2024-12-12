const std = @import("std");

const info = std.debug.print;

const Coord = std.math.Complex(i32);
const directions = [_]Coord{
    Coord.init(1, 0),
    Coord.init(-1, 0),
    Coord.init(0, 1),
    Coord.init(0, -1),
};

var area: std.ArrayList([]u8) = undefined;

inline fn in_bounds(x: anytype, y: anytype) bool {
    return !(x < 0 or x >= area.items[0].len or y < 0 or y >= area.items.len);
}

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(i32, @intCast(x)), @as(i32, @intCast(y)));
}

inline fn get(pos: Coord) ?u8 {
    if (!in_bounds(pos.re, pos.im))
        return null;
    return area.items[@as(usize, @intCast(pos.im))][@as(usize, @intCast(pos.re))];
}

inline fn put(pos: Coord, value: u8) void {
    area.items[@as(usize, @intCast(pos.im))][@as(usize, @intCast(pos.re))] = value;
}

const Wall = struct {
    in: Coord,
    out: Coord,
    fn shift(self: *Wall, dir: Coord) void {
        self.in = self.in.add(dir);
        self.out = self.out.add(dir);
    }

    fn trace(self: *Wall) Trace {
        const dir = self.in.sub(self.out).mulbyi().neg();
        const idx = get(self.in).?;
        var back = Trace{ .w = self.*, .idx = idx, .direction = dir.neg() };
        while (back.next()) |next|
            self.* = next;
        return Trace{ .w = self.*, .idx = idx, .direction = dir };
    }

    const Trace = struct {
        w: Wall,
        idx: u8,
        direction: Coord,
        fn next(self: *Trace) ?Wall {
            if (get(self.w.in) != self.idx or get(self.w.out) == self.idx)
                return null;
            defer self.w.shift(self.direction);
            return self.w;
        }
    };
};

fn fill(here: Coord, crop: u8, allocator: std.mem.Allocator) ![2]usize {
    var queue = std.ArrayList(Coord).init(allocator);
    defer queue.deinit();
    try queue.append(here);
    var walls = std.AutoHashMap(Wall, void).init(allocator);
    defer walls.deinit();
    var region_area: usize = 0;
    while (queue.popOrNull()) |location| {
        if (get(location) != crop)
            continue;
        region_area += 1;
        inline for (directions) |direction| {
            const other = location.add(direction);
            try queue.append(other);
            try walls.putNoClobber(Wall{ .in = location, .out = other }, void{});
        }
        put(location, ~crop);
    }
    var perimeter1: usize = 0;
    var perimeter2: usize = 0;
    while (walls.count() > 0) {
        var iter = walls.keyIterator();
        var wall = iter.next().?.*;
        _ = walls.remove(wall);
        if (get(wall.out) == ~crop)
            continue;
        var trace = wall.trace();
        while (trace.next()) |next| {
            _ = walls.remove(next);
            perimeter1 += 1;
        }
        perimeter2 += 1;
    }

    return [2]usize{ region_area * perimeter1, region_area * perimeter2 };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    area = @TypeOf(area).init(allocator);
    defer {
        for (area.items) |line|
            allocator.free(line);
        area.deinit();
    }

    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |line|
        try area.append(line);

    var part1: usize = 0;
    var part2: usize = 0;
    for (area.items, 0..) |line, y|
        for (line, 0..) |crop, x| {
            if (crop < 128) {
                const result = try fill(c(x, y), crop, allocator);
                part1 += result[0];
                part2 += result[1];
            }
        };
    info("{}\n{}\n", .{ part1, part2 });
}
