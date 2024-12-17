const std = @import("std");

const info = std.debug.print;
const Reg = u64;
var regs = [_]Reg{ 0, 0, 0 };
var ptr: usize = 0;
var insns: std.ArrayList(u3) = undefined;
var output: std.ArrayList(u3) = undefined;

inline fn combo(value: u3) u3 {
    return switch (value) {
        0, 1, 2, 3 => value,
        4, 5, 6 => @as(u3, @intCast(regs[value - 4] & 7)),
        else => @panic("wrong combo operand"),
    };
}

fn step() !void {
    const operand = insns.items[ptr + 1];
    var nptr: ?usize = null;
    switch (insns.items[ptr]) {
        0 => regs[0] = regs[0] >> combo(operand),
        1 => regs[1] ^= operand,
        2 => regs[1] = combo(operand),
        3 => nptr = if (regs[0] == 0) null else operand,
        4 => regs[1] ^= regs[2],
        5 => output.append(combo(operand)) catch @panic("out"),
        6 => regs[1] = regs[0] >> combo(operand),
        7 => regs[2] = regs[0] >> combo(operand),
    }
    ptr = if (nptr) |n| n else ptr + 2;
}

fn run(a: Reg) !void {
    regs[0] = a;
    ptr = 0;
    output.clearRetainingCapacity();
    while (ptr < insns.items.len)
        try step();
}

const I = struct { offt: usize, value: Reg };
fn item_cmp(_: void, a: I, b: I) bool {
    return a.value > b.value;
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
        regs[idx] = try std.fmt.parseInt(Reg, pv.next().?, 10);
    }

    _ = try reader.readUntilDelimiterOrEof(&buf, '\n');

    insns = @TypeOf(insns).init(allocator);
    defer insns.deinit();
    output = @TypeOf(output).init(allocator);
    defer output.deinit();

    const line = (try reader.readUntilDelimiterOrEof(&buf, '\n')).?;
    var pi = std.mem.tokenizeAny(u8, line, ", ");
    _ = pi.next();
    while (pi.next()) |ins|
        try insns.append(try std.fmt.parseInt(u3, ins, 10));

    try run(regs[0]);
    for (output.items, 1..) |out, idx|
        info("{}{c}", .{ out, ",\n"[@intFromBool(idx == output.items.len)] });

    var queue = std.ArrayList(I).init(allocator);
    defer queue.deinit();

    try queue.append(I{ .offt = insns.items.len - 1, .value = 0 });
    while (queue.popOrNull()) |i| {
        for (0..8) |v| {
            const nv = i.value << 3 | v;
            try run(nv);
            if (std.mem.eql(u3, insns.items[i.offt..], output.items)) {
                if (i.offt == 0) {
                    info("{}\n", .{nv});
                    return;
                }
                try queue.append(I{ .offt = i.offt - 1, .value = nv });
            }
        }
        std.mem.sort(I, queue.items, {}, item_cmp);
    }
}
