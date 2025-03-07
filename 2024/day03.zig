const std = @import("std");

const info = std.debug.print;

const Parser = struct {
    data: []u8,
    idx: usize = 0,
    fn string(self: *Parser, str: []const u8) ?void {
        if (std.mem.startsWith(u8, self.data[self.idx..], str)) {
            self.idx += str.len;
            return;
        }
        return null;
    }
    fn _digit(self: *Parser) ?u8 {
        if (std.fmt.charToDigit(self.data[self.idx], 10)) |digit| {
            self.idx += 1;
            return digit;
        } else |_| return null;
    }
    fn number(self: *Parser) ?u64 {
        if (self._digit()) |first_digit| {
            var value: u64 = first_digit;
            while (self._digit()) |digit|
                value = value * 10 + digit;
            return value;
        }
        return null;
    }
    fn search(self: *Parser, comptime needles: []const []const u8) ?[]const u8 {
        var starts: [needles.len]u8 = undefined;
        for (needles, 0..) |needle, idx|
            starts[idx] = needle[0];
        while (std.mem.indexOfAnyPos(u8, self.data, self.idx, &starts)) |pos| {
            self.idx = pos;
            for (needles) |needle|
                if (self.string(needle)) |_|
                    return needle;
            self.idx += 1;
        }
        return null;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var data = std.ArrayList(u8).init(allocator);
    defer data.deinit();

    const reader = std.io.getStdIn().reader();
    try reader.readAllArrayList(&data, comptime (128 * 1024));

    var part1: u64 = 0;
    var part2: u64 = 0;
    var enabled = true;
    var parser = Parser{ .data = data.items };

    const Pattern = enum { @"mul(", @"do()", @"don't()" };
    while (parser.search(std.meta.fieldNames(Pattern))) |found|
        switch (std.meta.stringToEnum(Pattern, found).?) {
            .@"mul(" => {
                const num1 = parser.number() orelse continue;
                parser.string(",") orelse continue;
                const num2 = parser.number() orelse continue;
                parser.string(")") orelse continue;
                part1 += num1 * num2;
                if (enabled)
                    part2 += num1 * num2;
            },
            .@"do()" => enabled = true,
            .@"don't()" => enabled = false,
        };
    info("{}\n{}\n", .{ part1, part2 });
}
