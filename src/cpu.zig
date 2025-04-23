pub fn word(hi: u8, lo: u8) u16 {
    return @as(u16, hi) << 8 | @as(u16, lo);
}

pub const Machine = struct {
    memory: [0x1000]u8,
    vram: [64 * 32]bool,
    regs: [16]u8,
    I: u16,
    sound_timer: u8,
    delay_timer: u8,
    pc: u16,
    sp: u8,
    stack: [0x100]u16,

    op: Instruction,

    pub fn op_jp(self: *Machine) void {
        self.pc = self.op.nnn();
    }

    pub fn op_sen(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        if (self.regs[r] == kk) {
            self.pc += 2;
        }
    }

    pub fn op_snen(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        if (self.regs[r] != kk) {
            self.pc += 2;
        }
    }

    pub fn op_se(self: *Machine) void {
        const rx = self.op.x();
        const ry = self.op.y();
        if (self.regs[rx] == self.regs[ry]) {
            self.pc += 2;
        }
    }

    pub fn op_ldn(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        self.regs[r] = kk;
    }

    pub fn op_addn(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        const res = @addWithOverflow(self.regs[r], kk);
        self.regs[r] = res[0];
    }

    // 8xxx ALU ops

    pub fn op_ld(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] = self.regs[ry];
    }

    pub fn op_or(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] |= self.regs[ry];
    }

    pub fn op_and(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] &= self.regs[ry];
    }

    pub fn op_xor(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] ^= self.regs[ry];
    }

    pub fn op_add(self: *Machine, rx: u4, ry: u4) void {
        const res = @addWithOverflow(self.regs[rx], self.regs[ry]);
        self.regs[rx] = res[0];
        self.regs[0xf] = res[1];
    }

    pub fn op_sub(self: *Machine, rx: u4, ry: u4) void {
        const res = @subWithOverflow(self.regs[rx], self.regs[ry]);
        self.regs[rx] = res[0];
        self.regs[0xf] = ~res[1];
    }

    pub fn op_shr(self: *Machine, rx: u4, ry: u4) void {
        self.regs[0xf] = self.regs[rx] & 1;
        self.regs[rx] = self.regs[rx] >> 1;
    }

    pub fn op_subn(self: *Machine, rx: u4, ry: u4) void {
        const res = @subWithOverflow(self.regs[ry], self.regs[rx]);
        self.regs[rx] = res[0];
        self.regs[0xf] = ~res[1];
    }

    pub fn op_shl(self: *Machine, rx: u4, ry: u4) void {
        self.regs[0xf] = self.regs[rx] >> 7;
        self.regs[rx] = self.regs[rx] << 1;
    }

    pub fn op_sne(self: *Machine) void {
        const rx = self.op.x();
        const ry = self.op.y();
        if (self.regs[rx] != self.regs[ry]) {
            self.pc += 2;
        }
    }

    pub fn op_ldi(self: *Machine) void {
        
    }



    pub fn fetch(self: *Machine) u8 {
        const data = self.memory[self.pc];
        self.pc += 1;
        return data;
    }

    pub fn fetch16(self: *Machine) u16 {
        const hi = self.memory[self.pc];
        const lo = self.memory[self.pc + 1];
        self.pc += 2;
        return word(hi, lo);
    }

    pub fn step(self: *Machine) error{UnknownOp}!void {
        const hi = self.fetch();
        const lo = self.fetch();
        const op = Instruction{ .op = word(hi, lo) };
        self.op = op;

        switch (op.z()) {
            0x1 => self.op_jp(),
            0x8 => {
                const rx = self.op.x();
                const ry = self.op.y();
                switch (self.op.n) {
                    0x0 => self.op_ld(rx, ry),
                    0x1 => self.op_or(rx, ry),
                    0x2 => self.op_and(rx, ry),
                    0x3 => self.op_xor(rx, ry),
                    0x4 => self.op_add(rx, ry),
                    0x5 => self.op_sub(rx, ry),
                    0x6 => self.op_shr(rx, ry),
                    0x7 => self.op_subn(rx, ry),
                    0xE => self.op_shl(rx, ry),
                    else => return error.UnknownOp,
                }
            }
            else => return error.UnknownOp,
        }
    }
};

pub const Instruction = struct {
    op: u16,

    pub fn nnn(self: Instruction) u12 {
        return @intCast(self.op);
    }

    pub fn kk(self: Instruction) u8 {
        return @intCast(self.op);
    }

    pub fn z(self: Instruction) u4 {
        return @intCast(self.hi >> 12);
    }

    pub fn x(self: Instruction) u4 {
        return @intCast(self.hi >> 8);
    }

    pub fn y(self: Instruction) u4 {
        return @intCast(self.lo >> 4);
    }

    pub fn n(self: Instruction) u4 {
        return @intCast(self.lo);
    }
};
