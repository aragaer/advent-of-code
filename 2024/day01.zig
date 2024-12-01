const std = @import("std");

const info = std.debug.print;
const Allocator = std.mem.Allocator;
const List = std.ArrayList(u32);

fn get_input(allocator: Allocator) !struct { first: List, second: List } {
    const reader = std.io.getStdIn().reader();
    var buffer: [100]u8 = undefined;
    var first = List.init(allocator);
    var second = List.init(allocator);
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var splits = std.mem.tokenizeAny(u8, line, " \t");
        const f = try std.fmt.parseInt(u32, splits.next().?, 10);
        const s = try std.fmt.parseInt(u32, splits.next().?, 10);
        try first.append(f);
        try second.append(s);
    }
    return .{ .first = first, .second = second };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const data = try get_input(allocator);
    defer data.first.deinit();
    defer data.second.deinit();

    std.mem.sort(u32, data.first.items, {}, comptime std.sort.asc(u32));
    std.mem.sort(u32, data.second.items, {}, comptime std.sort.asc(u32));

    var secondMap = std.AutoHashMap(u32, u32).init(allocator);
    defer secondMap.deinit();

    var part1: u32 = 0;
    for (data.first.items, data.second.items) |f, s| {
        part1 += if (f > s) f - s else s - f;
        if (secondMap.get(s)) |cnt| {
            try secondMap.put(s, cnt + 1);
        } else {
            try secondMap.putNoClobber(s, 1);
        }
    }
    info("{}\n", .{part1});

    var part2: u32 = 0;
    for (data.first.items) |i|
        part2 += i * (secondMap.get(i) orelse 0);
    info("{}\n", .{part2});
}
