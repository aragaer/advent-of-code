const std = @import("std");

const info = std.debug.print;

const Coord = std.math.Complex(i32);

var initial_position: Coord = undefined;
var initial_direction: Coord = Coord.init(0, -1);
var area: std.ArrayList([]bool) = undefined;

inline fn get(pos: Coord) ?bool {
    if (pos.re < 0 or pos.re >= area.items.len)
        return null;
    if (pos.im < 0 or pos.im >= area.items.len)
        return null;
    return area.items[@as(usize, @intCast(pos.im))][@as(usize, @intCast(pos.re))];
}

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(i32, @intCast(x)), @as(i32, @intCast(y)));
}

fn walk_to_exit(allocator: std.mem.Allocator) !?std.AutoHashMap(Coord, Coord) {
    var position = initial_position;
    var direction = initial_direction;
    var visited = std.AutoHashMap(Coord, Coord).init(allocator);
    try visited.put(position, direction);
    while (get(position.add(direction))) |step_to| {
        if (step_to) {
            direction = direction.mulbyi();
            continue;
        }
        position = position.add(direction);
        if (visited.get(position)) |old_dir| {
            if (old_dir.re == direction.re and old_dir.im == direction.im) {
                visited.deinit();
                return null;
            }
        }
        try visited.put(position, direction);
    }
    return visited;
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

    var linenum: i32 = 0;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |buf| {
        defer allocator.free(buf);
        if (buf.len == 0)
            break;
        var line = try allocator.alloc(bool, buf.len);
        for (buf, 0..) |char, idx| {
            line[idx] = char == '#';
            if (char == '^')
                initial_position = c(idx, linenum);
        }
        try area.append(line);
        linenum += 1;
    }

    var part2: u32 = 0;
    var visited = (try walk_to_exit(allocator)).?;
    defer visited.deinit();
    var iter = visited.keyIterator();
    while (iter.next()) |pos| {
        if (pos.re == initial_position.re and pos.im == initial_position.im)
            continue;
        area.items[@as(usize, @intCast(pos.im))][@as(usize, @intCast(pos.re))] = true;
        var result = try walk_to_exit(allocator);
        if (result == null)
            part2 += 1
        else
            result.?.deinit();
        area.items[@as(usize, @intCast(pos.im))][@as(usize, @intCast(pos.re))] = false;
    }

    info("{}\n{}\n", .{ visited.count(), part2 });
}
