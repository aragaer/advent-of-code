const std = @import("std");

const info = std.debug.print;
const T = i32;
const Coord = std.math.Complex(T);
const Pt = struct { pos: Coord, dir: Coord };

var area: std.ArrayList([]bool) = undefined;
var seen: std.AutoHashMap(Pt, usize) = undefined;
var best: std.AutoHashMap(Coord, void) = undefined;
var queue: std.ArrayList(Pt) = undefined;
var width: usize = undefined;
var height: usize = undefined;

inline fn in_bounds(x: anytype, y: anytype) bool {
    return !(x < 0 or x >= area.items[0].len or y < 0 or y >= area.items.len);
}

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(T, @intCast(x)), @as(T, @intCast(y)));
}

inline fn get(pos: Coord) ?bool {
    if (!in_bounds(pos.re, pos.im))
        return null;
    return area.items[@as(usize, @intCast(pos.im))][@as(usize, @intCast(pos.re))];
}

fn dump() void {
    for (0..height) |y| {
        for (0..width) |x| {
            if (get(c(x, y)) orelse false)
                info("#", .{})
            else if (best.contains(c(x, y)))
                info("O", .{})
            else
                info(".", .{});
        }
        info("\n", .{});
    }
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
    seen = @TypeOf(seen).init(allocator);
    defer seen.deinit();
    queue = @TypeOf(queue).init(allocator);
    defer queue.deinit();
    best = @TypeOf(best).init(allocator);
    defer best.deinit();

    var buf: [200]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    var start: Coord = undefined;
    var finish: Coord = undefined;
    height = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var area_line = try allocator.alloc(bool, line.len);
        for (line, 0..) |char, idx| {
            area_line[idx] = char == '#';
            if (char == 'S')
                start = c(idx, height);
            if (char == 'E')
                finish = c(idx, height);
        }
        try area.append(area_line);
        height += 1;
        width = line.len;
    }

    try queue.append(Pt{ .pos = start, .dir = c(1, 0) });
    try seen.put(queue.getLast(), 0);

    var finishes = std.ArrayList(Pt).init(allocator);
    defer finishes.deinit();

    const DirCost = struct { Coord, usize };
    const dir_costs = [_]DirCost{ DirCost{ c(1, 0), 1 }, DirCost{ c(0, 1), 1001 }, DirCost{ c(0, -1), 1001 } };

    while (queue.popOrNull()) |pt| {
        if (get(pt.pos) != false)
            continue;
        const cost = seen.get(pt).?;
        if (pt.pos.re == finish.re and pt.pos.im == finish.im) {
            try finishes.append(pt);
            continue;
        }

        for (dir_costs) |dc| {
            const nd = pt.dir.mul(dc[0]);
            const npt = Pt{ .pos = pt.pos.add(nd), .dir = nd };
            const nc = cost + dc[1];
            if (seen.get(npt)) |seen_cost| {
                if (seen_cost < nc)
                    continue;
            }
            try queue.append(npt);
            try seen.put(npt, nc);
        }
    }
    var result: usize = seen.get(finishes.items[0]).?;
    for (finishes.items[1..]) |pt|
        result = @min(result, seen.get(pt).?);
    info("{}\n", .{result});

    for (finishes.items) |pt| {
        if (seen.get(pt) == result)
            try queue.append(pt);
    }

    try best.put(finish, void{});
    while (queue.popOrNull()) |pt| {
        const cost = seen.get(pt).?;
        for (dir_costs) |dc| {
            const nd = pt.dir.mul(dc[0]);
            const npt = Pt{ .pos = pt.pos.sub(pt.dir), .dir = nd };
            const nc = cost - dc[1];
            if (seen.get(npt) == nc) {
                try queue.append(npt);
                try best.put(npt.pos, void{});
            }
        }
    }

    info("{}\n", .{best.count()});
}
