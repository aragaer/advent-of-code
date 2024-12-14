const std = @import("std");

const info = std.debug.print;
const Coord = std.math.Complex(i32);

fn read() !?Params {
    var buf: [100]u8 = undefined;
    var data: Params = undefined;
    const reader = std.io.getStdIn().reader();
    for (0..3) |idx| {
        const line = try reader.readUntilDelimiterOrEof(&buf, '\n') orelse return null;
        var values = std.mem.tokenizeAny(u8, line, ":,");
        _ = values.next();
        data[idx * 2] = try std.fmt.parseInt(T, values.next().?[3..], 10);
        data[idx * 2 + 1] = try std.fmt.parseInt(T, values.next().?[3..], 10);
    }
    return data;
}

fn solve(params: Params) !?T {
    const lcq = params[0] * params[1] / std.math.gcd(params[0], params[1]);
    const t1 = lcq / params[0];
    const t2 = lcq / params[1];
    const b1 = params[3] * t2;
    const b2 = params[2] * t1;
    const p1 = params[5] * t2;
    const p2 = params[4] * t1;
    if ((b1 > b2) != (p1 > p2))
        return null;
    const b = if (b1 > b2) b1 - b2 else b2 - b1;
    const p = if (b1 > b2) p1 - p2 else p2 - p1;
    if (b == 0)
        @panic("woot!");
    if (@mod(p, b) == 0) {
        const snd = @divTrunc(p, b);
        const fst = @divTrunc(params[4] - params[2] * snd, params[0]);
        return fst * 3 + snd;
    }
    return null;
}

pub fn main() !void {
    var part1: T = 0;
    var part2: T = 0;
    while (try read()) |params| {
        part1 += try solve(params) orelse 0;
        var p2 = params;
        p2[4] += 10000000000000;
        p2[5] += 10000000000000;
        part2 += try solve(p2) orelse 0;
        _ = std.io.getStdIn().reader().readByte() catch break; // read empty line
    }
    info("{}\n{}\n", .{ part1, part2 });
}
