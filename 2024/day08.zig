const std = @import("std");

const info = std.debug.print;
const Coord = std.math.Complex(i32);

var data: std.ArrayList([]u8) = undefined;

inline fn in_bounds(x: anytype, y: anytype) bool {
    return !(x < 0 or x >= data.items.len or y < 0 or y >= data.items.len);
}

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(i32, @intCast(x)), @as(i32, @intCast(y)));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    data = @TypeOf(data).init(allocator);
    defer {
        for (data.items) |line|
            allocator.free(line);
        data.deinit();
    }

    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |buffer|
        try data.append(buffer);

    var freqs = std.AutoHashMap(u8, std.ArrayList(Coord)).init(allocator);
    defer {
        var lists = freqs.valueIterator();
        while (lists.next()) |list|
            list.deinit();
        freqs.deinit();
    }
    for (data.items, 0..) |line, y|
        for (line, 0..) |char, x| {
            if (char == '.')
                continue;
            var list = freqs.get(char) orelse std.ArrayList(Coord).init(allocator);
            try list.append(c(x, y));
            try freqs.put(char, list);
        };
    var antinodes = std.AutoHashMap(Coord, u8).init(allocator);
    defer antinodes.deinit();

    var iter = freqs.valueIterator();
    while (iter.next()) |list|
        for (list.items) |first|
            for (list.items) |second| {
                if (first.re == second.re and first.im == second.im)
                    continue;
                const diff = first.sub(second);
                if (@mod(diff.re, 3) == 0 and @mod(diff.im, 3) == 0)
                    info("possibly an antinode between {} and {}\n", .{ first, second });
                var anti = first;
                while (in_bounds(anti.re, anti.im)) : (anti = anti.add(diff))
                    try antinodes.put(anti, antinodes.get(anti) orelse 0);
                anti = first.add(diff);
                if (in_bounds(anti.re, anti.im))
                    try antinodes.put(anti, 1);
            };
    var part1: usize = 0;
    var anti_iter = antinodes.valueIterator();
    while (anti_iter.next()) |power|
        part1 += power.*;
    info("{}\n{}\n", .{ part1, antinodes.count() });
}
