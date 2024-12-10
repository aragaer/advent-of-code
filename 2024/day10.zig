const std = @import("std");

const info = std.debug.print;

const Coord = std.math.Complex(i32);
const directions = [_]Coord{
    Coord.init(1, 0),
    Coord.init(-1, 0),
    Coord.init(0, 1),
    Coord.init(0, -1),
};

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(i32, @intCast(x)), @as(i32, @intCast(y)));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var area = std.AutoHashMap(Coord, usize).init(allocator);
    defer area.deinit();
    var heads = std.ArrayList(Coord).init(allocator);
    defer heads.deinit();
    var tails = std.ArrayList(Coord).init(allocator);
    defer tails.deinit();

    var linenum: i32 = 0;
    const reader = std.io.getStdIn().reader();
    var buffer: [1024]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |slice| {
        for (slice, 0..) |ch, idx| {
            const height = std.fmt.charToDigit(ch, 10) catch @panic("not a digit");
            try area.put(c(idx, linenum), height);
            if (height == 0)
                try heads.append(c(idx, linenum));
            if (height == 9)
                try tails.append(c(idx, linenum));
        }
        linenum += 1;
    }

    var visited = std.AutoHashMap(Coord, usize).init(allocator);
    defer visited.deinit();
    var queue = std.fifo.LinearFifo(Coord, .Dynamic).init(allocator);
    defer queue.deinit();
    var part1: usize = 0;
    var part2: usize = 0;
    for (heads.items) |head| {
        visited.clearRetainingCapacity();
        try visited.put(head, 1);
        try queue.writeItem(head);
        while (queue.readItem()) |location| {
            const height = area.get(location) orelse @panic("outside");
            inline for (directions) |direction| {
                const next = location.add(direction);
                if (area.get(next) == height + 1) {
                    const seen = visited.get(next) orelse 0;
                    if (seen == 0 and height < 8)
                        try queue.writeItem(next);
                    try visited.put(next, seen + visited.get(location).?);
                }
            }
        }
        for (tails.items) |tail| {
            part1 += @intFromBool(visited.contains(tail));
            part2 += visited.get(tail) orelse 0;
        }
    }
    info("{}\n{}\n", .{ part1, part2 });
}
