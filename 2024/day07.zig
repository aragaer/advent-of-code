const std = @import("std");

const info = std.debug.print;

inline fn add(a: anytype, b: anytype) ?@TypeOf(a, b) {
    const result = @addWithOverflow(a, b);
    return if (result[1] == 0) result[0] else null;
}

inline fn mul(a: anytype, b: anytype) ?@TypeOf(a, b) {
    const result = @mulWithOverflow(a, b);
    return if (result[1] == 0) result[0] else null;
}

inline fn cat(a: anytype, b: anytype) ?@TypeOf(a, b) {
    const len = 1 + std.math.log10_int(b);
    const tens = if (std.math.powi(u64, 10, len)) |value|
        value
    else |_|
        return null;
    if (mul(a, tens)) |added|
        return add(added, b)
    else
        return null;
}

const Incorrect = [2]u64{ 0, 0 };

fn solve(total: u64, acc: u64, rest: std.ArrayList(u64), offt: usize) [2]u64 {
    if (offt == rest.items.len)
        return if (acc == total) [2]u64{ total, total } else Incorrect;
    if (acc > total)
        return Incorrect;
    const next = rest.items[offt];
    const accs = [_]?u64{ add(acc, next), mul(acc, next), cat(acc, next) };
    for (accs, 0..) |newacc, idx|
        if (newacc != null) {
            var result = solve(total, newacc.?, rest, offt + 1);
            if (result[1] == 0)
                continue;
            if (idx == 2)
                result[0] = 0;
            return result;
        };
    return Incorrect;
}

fn check(allocator: std.mem.Allocator, line: []const u8) ![2]u64 {
    var nums = std.mem.tokenizeAny(u8, line, ": ");
    const result = try std.fmt.parseInt(u64, nums.next().?, 10);
    var args = std.ArrayList(u64).init(allocator);
    defer args.deinit();
    while (nums.next()) |num|
        try args.append(try std.fmt.parseInt(u64, num, 10));
    return solve(result, args.items[0], args, 1);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var part1: u64 = 0;
    var part2: u64 = 0;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |buf| {
        defer allocator.free(buf);
        if (buf.len == 0)
            break;
        const results = try check(allocator, buf);
        part1 += results[0];
        part2 += results[1];
    }

    info("{}\n{}\n", .{ part1, part2 });
}
