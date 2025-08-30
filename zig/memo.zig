const std = @import("std");

pub const Memo = struct {
    const Node = struct {
        children: std.AutoHashMap(u64, *Node),
        has_value: bool,
        value: bool,

        fn init(allocator: std.mem.Allocator) !*Node {
            const node = try allocator.create(Node); // const: not mutated
            node.* = .{
                .children = std.AutoHashMap(u64, *Node).init(allocator),
                .has_value = false,
                .value = false,
            };
            return node;
        }
    };

    allocator: std.mem.Allocator,
    root: *Node,
    fun: *const fn ([]const u64) bool,

    pub fn init(
        allocator: std.mem.Allocator,
        fun: *const fn ([]const u64) bool,
    ) !Memo {
        const root = try Node.init(allocator);
        return .{
            .allocator = allocator,
            .root = root,
            .fun = fun,
        };
    }

    pub fn deinit(self: *Memo) void {
        self.freeNode(self.root);
    }

    fn freeNode(self: *Memo, node: *Node) void {
        var it = node.children.iterator();
        while (it.next()) |entry| {
            const child = entry.value_ptr.*;
            self.freeNode(child);
        }
        node.children.deinit();
        self.allocator.destroy(node);
    }

    pub fn call(self: *Memo, args: []const u64) !bool {
        var current = self.root;

        for (args) |k| {
            if (current.children.get(k)) |next_ptr| {
                current = next_ptr;
            } else {
                const new_node = try Node.init(self.allocator);
                try current.children.put(k, new_node);
                current = new_node;
            }
        }

        if (current.has_value) {
            return current.value;
        }

        const v = self.fun(args);
        current.value = v;
        current.has_value = true;
        return v;
    }
};
