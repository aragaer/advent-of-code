const std = @import("std");

const info = std.debug.print;
const T = i32;
const Coord = std.math.Complex(T);
const directions = [_]Coord{
    Coord.init(1, 0),
    Coord.init(-1, 0),
    Coord.init(0, 1),
    Coord.init(0, -1),
};

var area: std.AutoHashMap(Coord, ?usize) = undefined;

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(T, @intCast(x)), @as(T, @intCast(y)));
}

fn count_cheats(range: usize) usize {
    var result: usize = 0;
    const srange = @as(T, @intCast(range));

    var iter = area.iterator();
    while (iter.next()) |pt| {
        const here = pt.key_ptr.*;
        const cost = pt.value_ptr.*.?;
        if (cost == 0)
            continue;
        for (0..(range * 2 + 1)) |bdx| {
            const dx = @as(T, @intCast(bdx)) - srange;
            for (0..(range * 2 + 1)) |bdy| {
                const dy = @as(T, @intCast(bdy)) - srange;
                const dist = @abs(dx) + @abs(dy);
                if (dist > range)
                    continue;
                const cheat_cost = area.get(here.add(c(dx, dy))) orelse 0 orelse 0;
                if (cheat_cost > 0 and cheat_cost + dist + 100 <= cost)
                    result += 1;
            }
        }
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    area = @TypeOf(area).init(allocator);
    defer area.deinit();
    var next: ?struct { Coord, usize } = null;

    var buf: [200]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    var y: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| : (y += 1)
        for (line, 0..) |char, x| {
            const here = c(x, y);
            try area.put(here, switch (char) {
                '#' => 0,
                'E' => 1,
                else => null,
            });
            if (char == 'E')
                next = .{ here, 1 };
        };

    while (next) |pt| {
        next = null;
        inline for (directions) |dir| {
            const np = pt[0].add(dir);
            const npt = area.getPtr(np).?;
            if (npt.* == null) {
                npt.* = pt[1] + 1;
                next = .{ np, pt[1] + 1 };
            }
        }
    }

    info("{}\n{}\n", .{ count_cheats(2), count_cheats(20) });
}
