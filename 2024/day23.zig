const std = @import("std");

const info = std.debug.print;
const Id = usize;
const Key = usize;
const Name = []const u8;

const Node = struct {
    name: Name,
    neighbours: std.AutoHashMap(usize, void),

    const This = @This();

    fn init(allocator: std.mem.Allocator, name: Name) This {
        const my_name = name;
        const neighs = std.AutoHashMap(usize, void).init(allocator);
        return Node{ .neighbours = neighs, .name = my_name };
    }

    fn deinit(self: *This) void {
        self.neighbours.allocator.free(self.name);
        self.neighbours.deinit();
    }

    inline fn append(self: *This, other: Id) !void {
        try self.neighbours.put(other, void{});
    }
};

var nodes: std.ArrayList(Node) = undefined;
var links: std.AutoHashMap(Key, void) = undefined;

inline fn mkKey(f: Id, s: Id) Key {
    return f << 10 | s;
}

inline fn unKey(k: Key) [2]Id {
    return .{ k >> 10, k & 1023 };
}

fn nameLessThan(_: void, f: Name, s: Name) bool {
    return std.mem.lessThan(u8, f, s);
}

fn solve_part1() !void {
    var triples = std.StringHashMap(void).init(nodes.allocator);
    defer {
        var keys = triples.keyIterator();
        while (keys.next()) |key|
            nodes.allocator.free(key.*);
        triples.deinit();
    }

    var linkiter = links.keyIterator();
    while (linkiter.next()) |link| {
        const ks = unKey(link.*);
        if (ks[0] > ks[1])
            continue;
        var iter = nodes.items[ks[0]].neighbours.keyIterator();
        while (iter.next()) |nf| {
            if (!links.contains(mkKey(nf.*, ks[1])))
                continue;
            var triple: [3]Name = undefined;
            var have_t = false;
            for ([3]Id{ ks[0], ks[1], nf.* }, 0..) |i, idx| {
                triple[idx] = nodes.items[i].name;
                have_t = have_t or triple[idx][0] == 't';
            }
            if (!have_t)
                continue;
            std.mem.sort(Name, &triple, {}, nameLessThan);
            const sorted = try std.mem.join(nodes.allocator, ",", &triple);
            if ((try triples.getOrPut(sorted)).found_existing)
                nodes.allocator.free(sorted);
        }
    }
    info("{}\n", .{triples.count()});
}

const NodeSet = std.ArrayList(Id);

// based on
// https://www.geeksforgeeks.org/maximal-clique-problem-recursive-solution/
fn bronKerbosch(cl: *NodeSet, cd: *NodeSet, ch: *NodeSet, best: *NodeSet) !void {
    defer cd.deinit();
    defer ch.deinit();
    if (cd.items.len == 0 and ch.items.len == 0) {
        if (cl.items.len > best.items.len) {
            best.deinit();
            best.* = cl.*;
        } else cl.deinit();
        return;
    }
    defer cl.deinit();

    while (cd.popOrNull()) |candidate| {
        var ncl = try cl.clone();
        try ncl.append(candidate);
        var ncd = NodeSet.init(cl.allocator);
        for (cd.items) |other| {
            if (links.contains(mkKey(candidate, other)))
                try ncd.append(other);
        }
        var nch = NodeSet.init(cl.allocator);
        for (ch.items) |other| {
            if (links.contains(mkKey(candidate, other)))
                try nch.append(other);
        }
        bronKerbosch(&ncl, &ncd, &nch, best) catch @panic("recur");
        try ch.append(candidate);
    }
}

fn solve_part2() !void {
    var e1 = NodeSet.init(nodes.allocator);
    var e2 = NodeSet.init(nodes.allocator);
    var clique = NodeSet.init(nodes.allocator);
    var all = NodeSet.init(nodes.allocator);
    for (0..nodes.items.len) |idx|
        try all.append(idx);

    bronKerbosch(&e1, &all, &e2, &clique) catch @panic("recur");
    defer clique.deinit();
    var buf = std.ArrayList(Name).init(nodes.allocator);
    defer buf.deinit();
    for (clique.items) |node|
        try buf.append(nodes.items[node].name);
    std.mem.sort(Name, buf.items, {}, nameLessThan);
    info("{s}", .{buf.items[0]});
    for (buf.items[1..]) |key|
        info(",{s}", .{key});
    info("\n", .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    nodes = @TypeOf(nodes).init(allocator);
    defer {
        for (nodes.items) |*node|
            node.deinit();
        nodes.deinit();
    }
    links = @TypeOf(links).init(allocator);
    defer links.deinit();

    var seen = std.StringHashMap(usize).init(allocator);
    defer seen.deinit();

    var buf: [10]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var ids: [2]Id = undefined;
        for (0..2) |idx| {
            const n = try allocator.dupe(u8, line[(idx * 3)..(idx * 3 + 2)]);
            const f = try seen.getOrPut(n);
            if (f.found_existing)
                allocator.free(n)
            else {
                f.value_ptr.* = nodes.items.len;
                try nodes.append(Node.init(allocator, n));
            }
            ids[idx] = f.value_ptr.*;
        }
        try links.put(mkKey(ids[0], ids[1]), void{});
        try links.put(mkKey(ids[1], ids[0]), void{});
        try nodes.items[ids[0]].append(ids[1]);
        try nodes.items[ids[1]].append(ids[0]);
    }

    try solve_part1();
    try solve_part2();
}
