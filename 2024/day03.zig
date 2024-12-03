const std = @import("std");

const info = std.debug.print;

const Parser = struct {
    data: []u8,
    idx: usize = 0,
    fn string(self: *Parser, str: []const u8) ?void {
        const len = str.len;
        if (std.mem.eql(u8, self.data[self.idx .. self.idx + len], str)) {
            self.idx += len;
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
    fn seek(self: *Parser, needles: []const u8) ?u8 {
        if (std.mem.indexOfAnyPos(u8, self.data, self.idx, needles)) |pos| {
            self.idx = pos + 1;
            return self.data[pos];
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
    while (parser.seek("md")) |found|
        switch (found) {
            'm' => {
                parser.string("ul(") orelse continue;
                const num1 = parser.number() orelse continue;
                parser.string(",") orelse continue;
                const num2 = parser.number() orelse continue;
                parser.string(")") orelse continue;
                part1 += num1 * num2;
                if (enabled)
                    part2 += num1 * num2;
            },
            'd' => {
                if (parser.string("o()")) |_|
                    enabled = true;
                if (parser.string("on't()")) |_|
                    enabled = false;
            },
            else => @panic("should be either m or d only"),
        };
    info("{}\n{}\n", .{ part1, part2 });
}
