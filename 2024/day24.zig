const std = @import("std");

const info = std.debug.print;
const Name = []const u8;

var names: std.StringHashMap(usize) = undefined;
var bits: [3][]usize = undefined;
var signals: std.AutoHashMap(usize, u1) = undefined;

fn mkId(name: Name) Name {
    const cnt = names.count();
    if (names.contains(name))
        return names.getKey(name).?;
    const key = names.allocator.dupe(u8, name) catch @panic("mem");
    names.put(key, cnt) catch @panic("mem");
    return key;
}

fn getId(name: Name) usize {
    return names.get(name).?;
}

fn nameLessThan(_: void, f: Name, s: Name) bool {
    return std.mem.lessThan(u8, f, s);
}

fn nameGtThan(_: void, f: Name, s: Name) bool {
    return std.mem.lessThan(u8, s, f);
}

const Op = enum { AND, OR, XOR };
const Node = struct {
    id: usize,
    name: Name,
    op: ?Op,
    children: ?[2]usize,
};
var nodes: std.ArrayList(Node) = undefined;

fn get_signal(id: usize) u1 {
    if (signals.get(id)) |signal|
        return signal;
    const node = nodes.items[id];
    const v1 = get_signal(node.children.?[0]);
    const v2 = get_signal(node.children.?[1]);
    const res = switch (node.op.?) {
        .AND => v1 & v2,
        .OR => v1 | v2,
        .XOR => v1 ^ v2,
    };
    signals.put(id, res) catch @panic("mem");
    return res;
}

fn calc() usize {
    var res: usize = 0;
    for (bits[2]) |zname|
        res = (res << 1) | @as(usize, @intCast(get_signal(zname)));
    return res;
}

fn fill_name_ids(prefix: u8) ![]usize {
    var collect = std.ArrayList(Name).init(names.allocator);
    defer collect.deinit();
    var iter = names.keyIterator();
    while (iter.next()) |name| {
        if (name.*[0] == prefix)
            try collect.append(name.*);
    }
    std.mem.sort(Name, collect.items, {}, nameGtThan);
    var result = try names.allocator.alloc(usize, collect.items.len);
    for (collect.items, 0..) |name, idx|
        result[idx] = getId(name);
    return result;
}

fn read_data(allocator: std.mem.Allocator) !void {
    var buf: [100]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0)
            break;

        var pv = std.mem.tokenizeSequence(u8, line, ": ");
        const name = mkId(pv.next().?);
        const id = getId(name);
        try signals.put(id, try std.fmt.parseInt(u1, pv.next().?, 2));
        try nodes.append(Node{ .id = id, .name = name, .op = null, .children = null });
    }

    var unprocessed = std.ArrayList(struct { [3]usize, Op, Name }).init(allocator);
    defer unprocessed.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var pv = std.mem.tokenizeScalar(u8, line, ' ');
        const id1 = getId(mkId(pv.next().?));
        const op = std.meta.stringToEnum(Op, pv.next().?).?;
        const id2 = getId(mkId(pv.next().?));
        _ = pv.next().?;
        const name = mkId(pv.next().?);
        const id = getId(name);
        try unprocessed.append(.{ .{ id1, id2, id }, op, name });
    }

    var more = try allocator.alloc(Node, unprocessed.items.len);
    for (unprocessed.items) |r| {
        const id1 = r[0][0];
        const id2 = r[0][1];
        const id3 = r[0][2];
        more[id3 - nodes.items.len] = Node{ .id = id3, .name = r[2], .op = r[1], .children = .{ id1, id2 } };
    }
    try nodes.appendSlice(more);
    allocator.free(more);
}

var fixed: std.ArrayList(Name) = undefined;

fn print_node(id: usize) void {
    const node = nodes.items[id];
    if (node.children == null) {
        info("{s} ({}) is input\n", .{ node.name, id });
        return;
    }
    const c1 = node.children.?[0];
    const c2 = node.children.?[1];
    const n1 = nodes.items[c1];
    const n2 = nodes.items[c2];
    info("{s} ({}) = {s} ({}) {} {s} ({})\n", .{ node.name, id, n1.name, c1, node.op.?, n2.name, c2 });
}

fn swap(id1: usize, id2: usize) void {
    const tmp = nodes.items[id1];
    nodes.items[id1].children = nodes.items[id2].children;
    nodes.items[id2].children = tmp.children;
    nodes.items[id1].op = nodes.items[id2].op;
    nodes.items[id2].op = tmp.op;
    fixed.append(nodes.items[id1].name) catch @panic("mem");
    fixed.append(nodes.items[id2].name) catch @panic("mem");
}

fn find(arg1: usize, arg2: ?usize, op: ?Op) ?usize {
    for (nodes.items) |node| {
        if (node.children == null)
            continue;
        const c = node.children.?;
        if (c[0] != arg1 and c[1] != arg1)
            continue;
        if (arg2 != null and c[0] != arg2.? and c[1] != arg2.?)
            continue;
        if (op != null and node.op != op)
            continue;
        return node.id;
    }
    return null;
}

fn find_or_fix(arg1: usize, arg2: usize, op: Op) [3]usize {
    if (find(arg1, arg2, op)) |good|
        return .{ good, arg1, arg2 };
    const other = find(arg1, null, op) orelse find(arg2, null, op) orelse @panic("huh?");
    const c = nodes.items[other].children.?;
    if (c[0] == arg1) {
        swap(arg2, c[1]);
        return .{ other, arg1, c[1] };
    } else if (c[1] == arg1) {
        swap(arg2, c[0]);
        return .{ other, arg1, c[0] };
    } else if (c[0] == arg2) {
        swap(arg1, c[1]);
        return .{ other, c[1], arg2 };
    } else if (c[1] == arg2) {
        swap(arg1, c[0]);
        return .{ other, c[0], arg2 };
    }
    unreachable;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    names = @TypeOf(names).init(allocator);
    defer {
        var iter = names.keyIterator();
        while (iter.next()) |name|
            allocator.free(name.*);
        names.deinit();
    }
    signals = @TypeOf(signals).init(allocator);
    defer signals.deinit();
    nodes = @TypeOf(nodes).init(allocator);
    defer nodes.deinit();

    try read_data(allocator);

    for ("xyz", 0..) |prefix, idx|
        bits[idx] = try fill_name_ids(prefix);
    defer for (bits) |b|
        allocator.free(b);

    info("{}\n", .{calc()});

    fixed = @TypeOf(fixed).init(allocator);
    defer fixed.deinit();

    const register_cnt = bits[0].len;
    var idx = register_cnt;
    var carry: usize = undefined;
    while (idx > 0) : (idx -= 1) {
        const x = bits[0][idx - 1];
        const y = bits[1][idx - 1];
        const z = bits[2][idx];
        const o1 = find(x, y, Op.XOR).?;
        var out: usize = undefined;
        if (idx == register_cnt) {
            out = o1;
            carry = find(x, y, Op.AND).?;
            continue;
        }
        const res1 = find_or_fix(o1, carry, Op.XOR);
        carry = res1[2];
        out = res1[0];
        const res2 = find_or_fix(res1[1], res1[2], Op.AND);
        const res3 = find_or_fix(find(x, y, Op.AND).?, res2[0], Op.OR);
        carry = res3[0];
        out = find(res1[1], res1[2], Op.XOR).?;
        if (out != z) {
            swap(z, out);
            out = z;
            carry = find(res3[1], res3[2], Op.OR).?;
        }
    }

    std.mem.sort(Name, fixed.items, {}, nameLessThan);
    info("{s}", .{fixed.items[0]});
    for (fixed.items[1..]) |key|
        info(",{s}", .{key});
    info("\n", .{});
}
