const std = @import("std");

const info = std.debug.print;
const Reg = u64;
var regs = [_]Reg{ 0, 0, 0 };
var ptr: usize = 0;
var insns: std.ArrayList(u3) = undefined;

inline fn combo(value: u3) u3 {
    return switch (value) {
        0...3 => value,
        4...6 => @as(u3, @intCast(regs[value - 4] & 7)),
        else => @panic("wrong combo operand"),
    };
}

fn step() ?u3 {
    const operand = insns.items[ptr + 1];
    var nptr: ?usize = null;
    defer ptr = if (nptr) |n| n else ptr + 2;
    switch (insns.items[ptr]) {
        0 => regs[0] = regs[0] >> combo(operand),
        1 => regs[1] ^= operand,
        2 => regs[1] = combo(operand),
        3 => nptr = if (regs[0] == 0) null else operand,
        4 => regs[1] ^= regs[2],
        5 => return combo(operand),
        6 => regs[1] = regs[0] >> combo(operand),
        7 => regs[2] = regs[0] >> combo(operand),
    }
    return null;
}

fn run() ?u3 {
    while (ptr < insns.items.len)
        return step() orelse continue;
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var buf: [200]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    for (0..3) |idx| {
        const line = (try reader.readUntilDelimiterOrEof(&buf, '\n')).?;
        var pv = std.mem.tokenizeScalar(u8, line, ' ');
        _ = pv.next();
        _ = pv.next();
        regs[idx] = try std.fmt.parseInt(Reg, pv.rest(), 10);
    }

    _ = try reader.readUntilDelimiterOrEof(&buf, '\n');

    insns = @TypeOf(insns).init(allocator);
    defer insns.deinit();

    const line = (try reader.readUntilDelimiterOrEof(&buf, '\n')).?;
    var pi = std.mem.tokenizeAny(u8, line, ", ");
    _ = pi.next();
    while (pi.next()) |ins|
        try insns.append(try std.fmt.parseInt(u3, ins, 10));

    info("{?}", .{run()});
    while (run()) |out|
        info(",{}", .{out});
    info("\n", .{});

    var queue = std.ArrayList(struct { Reg, usize }).init(allocator);
    defer queue.deinit();

    try queue.append(.{ 0, 1 });
    outer: while (queue.popOrNull()) |i| {
        inline for (0..8) |v| {
            const nv = i[0] << 3 | (7 - v);
            regs[0] = nv;
            ptr = 0;
            if (run() == insns.items[insns.items.len - i[1]]) {
                try queue.append(.{ nv, i[1] + 1 });
                if (i[1] == insns.items.len)
                    break :outer;
            }
        }
    }
    info("{}\n", .{queue.pop()[0]});
}
