const std = @import("std");

const info = std.debug.print;

const Rule = struct { before: u32, after: u32 };
const Pages = std.ArrayList(u32);

var rules: std.AutoHashMap(Rule, void) = undefined;

inline fn is_bad(first: u32, second: u32) bool {
    return rules.contains(Rule{ .before = second, .after = first });
}

fn is_good(context: void, first: u32, second: u32) bool {
    _ = context;
    return !is_bad(first, second);
}

fn is_ordered(pages: Pages) bool {
    var pairs = std.mem.window(u32, pages.items, 2, 1);
    while (pairs.next()) |pair|
        if (is_bad(pair[0], pair[1]))
            return false;
    return true;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    rules = @TypeOf(rules).init(allocator);
    defer rules.deinit();

    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 10)) |buffer| {
        defer allocator.free(buffer);
        if (buffer.len == 0)
            break;
        var nums = std.mem.tokenizeScalar(u8, buffer, '|');
        const first = try std.fmt.parseInt(u32, nums.next().?, 10);
        const second = try std.fmt.parseInt(u32, nums.next().?, 10);
        try rules.putNoClobber(Rule{ .before = first, .after = second }, void{});
    }

    var part1: u32 = 0;
    var part2: u32 = 0;
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 100)) |buffer| {
        defer allocator.free(buffer);
        var pages = Pages.init(allocator);
        defer pages.deinit();
        var nums = std.mem.tokenizeScalar(u8, buffer, ',');
        while (nums.next()) |num|
            try pages.append(try std.fmt.parseInt(u32, num, 10));
        const middle = (pages.items.len - 1) / 2;
        if (is_ordered(pages)) {
            part1 += pages.items[middle];
        } else {
            std.mem.sort(u32, pages.items, {}, is_good);
            part2 += pages.items[middle];
        }
    }
    info("{}\n{}\n", .{ part1, part2 });
}
