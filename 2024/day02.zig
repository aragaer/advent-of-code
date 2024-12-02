const std = @import("std");

const Report = std.ArrayList(i32);

fn get_report(allocator: std.mem.Allocator) !Report {
    const reader = std.io.getStdIn().reader();
    if (reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 100)) |buffer| {
        const line = buffer orelse return error.Exhausted;
        var words = std.mem.tokenizeAny(u8, line, " \t");
        var report = Report.init(allocator);
        while (words.next()) |word|
            try report.append(try std.fmt.parseInt(i32, word, 10));
        allocator.free(line);
        return report;
    } else |err| return err;
}

inline fn is_ok(f: i32, s: i32, asc: bool) bool {
    return (f < s) == asc and f != s and @abs(f - s) < 4;
}

fn check_report(report: Report, skip: ?usize) !bool {
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
        else if (skip == null) {
            const to_try = [3]usize{ 0, idx - 1, idx };
            for (to_try[(if (idx == 2) 0 else 1)..]) |try_skip|
                if (try check_report(report, try_skip))
                    return false;
            return error.NotSafe;
        } else return false;
    }
    return true;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var result = [2]usize{ 0, 0 };
    while (get_report(allocator)) |report| {
        if (check_report(report, null)) |safe|
            result[@intFromBool(safe)] += 1
        else |_| {}
        report.deinit();
    } else |_| {}
    std.debug.print("{}\n{}\n", .{ result[1], result[0] + result[1] });
}
