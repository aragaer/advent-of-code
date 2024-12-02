const std = @import("std");

const info = std.debug.print;
const Allocator = std.mem.Allocator;
const List = std.ArrayList(i32);
const Parser = std.fmt.parseInt;

fn get_input(allocator: Allocator) !std.ArrayList(List) {
    const reader = std.io.getStdIn().reader();
    var buffer: [100]u8 = undefined;
    var result = std.ArrayList(List).init(allocator);
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var words = std.mem.tokenizeAny(u8, line, " \t");
        var report = List.init(allocator);
        while (words.next()) |word|
            try report.append(try Parser(i32, word, 10));
        try result.append(report);
    }
    return result;
}

fn is_ok(f: i32, s: i32, asc: bool) bool {
    return (f < s) == asc and f != s and @abs(f - s) < 4;
}

fn check_report(report: std.ArrayList(i32), skip: i32) usize {
    const checking = if (skip == 0) report.items[1..] else report.items;
    const asc = if (skip == 1)
        checking[0] < checking[2]
    else
        checking[0] < checking[1];
    var prev = checking[0];
    for (checking[1..], 1..) |this, idx| {
        if (idx == skip)
            continue;
        if (is_ok(prev, this, asc))
            prev = this
        else if (skip == -1) {
            const bad = @as(i32, @intCast(idx));
            for (@max(bad - 2, 0)..(idx + 1)) |try_skip|
                if (check_report(report, @as(i32, @intCast(try_skip))) == 0)
                    return 1;
            return 2;
        } else return 1;
    }
    return 0;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const data = try get_input(allocator);
    defer {
        for (data.items) |item|
            item.deinit();
        data.deinit();
    }

    var okay: u32 = 0;
    var fixed: u32 = 0;
    for (data.items) |report| {
        switch (check_report(report, -1)) {
            0 => okay += 1,
            1 => fixed += 1,
            else => {},
        }
    }
    info("{}\n{}\n", .{ okay, okay + fixed });
}
