const std = @import("std");

const info = std.debug.print;
const T = usize;
const Params = [6]T;

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

fn solve(params: Params) !?[2]T {
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
        return [2]T{ fst, snd };
    }
    return null;
}

pub fn main() !void {
    var part1: T = 0;
    var part2: T = 0;
    while (try read()) |params| {
        if (try solve(params)) |result| {
            if (result[0] <= 100 and result[1] <= 100)
                part1 += result[0] * 3 + result[1];
        }
        var p2 = params;
        p2[4] += 10000000000000;
        p2[5] += 10000000000000;
        if (try solve(p2)) |result|
            part2 += result[0] * 3 + result[1];
        _ = std.io.getStdIn().reader().readByte() catch break; // read empty line
    }
    info("{}\n{}\n", .{ part1, part2 });
}
