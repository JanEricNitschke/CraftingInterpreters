use super::{rules::Precedence, Compiler, LoopState};

use crate::{
    chunk::{CodeOffset, OpCode},
    scanner::TokenKind as TK,
    types::Line,
};

impl<'a> Compiler<'a> {
    pub(super) fn advance(&mut self) {
        self.previous = std::mem::take(&mut self.current);
        loop {
            let token = self.scanner.scan();
            self.current = Some(token);
            if !self.check(TK::Error) {
                break;
            }
            self.error_at_current(&self.current.as_ref().unwrap().as_str().to_string());
        }
    }

    pub(super) fn consume(&mut self, kind: TK, msg: &str) {
        if self.check(kind) {
            self.advance();
            return;
        }
        self.error_at_current(msg);
    }

    pub(super) fn line(&self) -> Line {
        match self.previous.as_ref() {
            Some(x) => x.line,
            None => Line(0),
        }
    }

    pub(super) fn match_(&mut self, kind: TK) -> bool {
        if !self.check(kind) {
            return false;
        }
        self.advance();
        true
    }

    pub(super) fn current_token_kind(&self) -> Option<TK> {
        self.current.as_ref().map(|t| t.kind)
    }

    pub(super) fn check(&self, kind: TK) -> bool {
        return self
            .current_token_kind()
            .map(|k| k == kind)
            .unwrap_or(false);
    }

    pub(super) fn check_previous(&self, kind: TK) -> bool {
        self.previous
            .as_ref()
            .map(|t| t.kind == kind)
            .unwrap_or(false)
    }

    pub(super) fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block(&mut self) {
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            self.declaration();
        }

        self.consume(TK::RightBrace, "Expect '}' after block.")
    }

    fn var_declaration(&mut self, mutable: bool) {
        let global = self.parse_variable("Expect variable name.", mutable);

        if self.match_(TK::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil, self.line());
        }

        self.consume(TK::Semicolon, "Expect ';' after variable declaration.");

        self.define_variable(global, mutable);
    }

    fn expression_statement(&mut self) {
        let line = self.line();
        self.expression();
        self.consume(TK::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop, line);
    }

    fn continue_statement(&mut self) {
        match self.loop_state.clone() {
            None => self.error("'continue' outside a loop."),
            Some(state) => {
                let line = self.line();
                self.consume(TK::Semicolon, "Expect ';' after 'continue'.");

                let locals_to_drop = self
                    .locals
                    .iter()
                    .rev()
                    .take_while(|local| local.depth > state.depth)
                    .count();
                for _ in 0..locals_to_drop {
                    self.emit_byte(OpCode::Pop, line);
                }
                self.emit_loop(state.start);
            }
        }
    }

    fn break_statement(&mut self) {
        match self.loop_state.clone() {
            None => self.error("'break' outside a loop."),
            Some(mut state) => {
                let line = self.line();
                self.consume(TK::Semicolon, "Expect ';' after 'break'.");

                let locals_to_drop = self
                    .locals
                    .iter()
                    .rev()
                    .take_while(|local| local.depth > state.depth)
                    .count();
                for _ in 0..locals_to_drop {
                    self.emit_byte(OpCode::Pop, line);
                }
                state.break_jumps.push(self.emit_jump(OpCode::Jump));
                self.loop_state = Some(state);
            }
        }
    }

    pub(super) fn declaration(&mut self) {
        if self.match_(TK::Var) {
            self.var_declaration(true);
        } else if self.match_(TK::Const) {
            self.var_declaration(false)
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_(TK::Print) {
            self.print_statement();
        } else if self.match_(TK::For) {
            self.for_statement();
        } else if self.match_(TK::If) {
            self.if_statement();
        } else if self.match_(TK::While) {
            self.while_statement();
        } else if self.match_(TK::Switch) {
            self.switch_statement();
        } else if self.match_(TK::Continue) {
            self.continue_statement();
        } else if self.match_(TK::Break) {
            self.break_statement();
        } else if self.match_(TK::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn if_statement(&mut self) {
        let line = self.line();
        self.consume(TK::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TK::RightParen, "Expect ')' after condition");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop, line);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        self.emit_byte(OpCode::Pop, line);
        if self.match_(TK::Else) {
            self.statement()
        }

        self.patch_jump(else_jump);
    }

    fn print_statement(&mut self) {
        let line = self.line();
        self.expression();
        self.consume(TK::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print, line);
    }

    fn while_statement(&mut self) {
        let line = self.line();
        let old_loop_state = {
            let start = CodeOffset(self.current_chunk_len());
            std::mem::replace(
                &mut self.loop_state,
                Some(LoopState {
                    depth: self.scope_depth,
                    start: start,
                    break_jumps: Vec::new(),
                }),
            )
        };
        self.consume(TK::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TK::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop, line);
        self.statement();
        self.emit_loop(self.loop_state.as_ref().unwrap().start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop, line);
        self.patch_break_jumps();
        self.loop_state = old_loop_state;
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TK::LeftParen, "Expect '(' after 'for'.");

        // Initializer
        if self.match_(TK::Semicolon) {
            // No initializer
        } else if self.match_(TK::Var) {
            self.var_declaration(true);
        } else if self.match_(TK::Const) {
            // This doesn't seem useful but I won't stop you
            self.var_declaration(false);
        } else {
            self.expression_statement();
        }

        let old_loop_state = {
            let start = CodeOffset(self.current_chunk_len());
            std::mem::replace(
                &mut self.loop_state,
                Some(LoopState {
                    depth: self.scope_depth,
                    start: start,
                    break_jumps: Vec::new(),
                }),
            )
        };

        let mut exit_jump = None;
        if !self.match_(TK::Semicolon) {
            self.expression();
            self.consume(TK::Semicolon, "Expect ';' after loop condition.");

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit_byte(OpCode::Pop, self.line());
        }

        // Increment
        if !self.match_(TK::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = CodeOffset(self.current_chunk_len());
            self.expression();
            self.emit_byte(OpCode::Pop, self.line());
            self.consume(TK::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(self.loop_state.as_ref().unwrap().start);
            self.loop_state.as_mut().unwrap().start = increment_start;
            self.patch_jump(body_jump)
        }

        self.statement();
        self.emit_loop(self.loop_state.as_ref().unwrap().start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::Pop, self.line());
        }

        self.patch_break_jumps();

        self.loop_state = old_loop_state;
        self.end_scope();
    }

    fn switch_statement(&mut self) {
        self.consume(TK::LeftParen, "Expect '(' after 'switch'");
        self.expression();
        self.consume(TK::RightParen, "Expect ')' after 'switch' value.");
        self.consume(TK::LeftBrace, "Expect '{' before 'switch' body.");

        let mut end_jumps = vec![];
        let mut had_default = false;

        // Loop cases and default until the file ends or  the switch gets closed
        while !self.check(TK::RightBrace) && !self.check(TK::Eof) {
            if had_default {
                self.error_at_current("No 'case' or 'default' allowed after 'default' branch.");
            }

            // Check condition and build the jump over the case if false
            let miss_jump = if self.match_(TK::Case) {
                // Have to dup because equality check removes it
                self.emit_byte(OpCode::Dup, self.line());
                self.expression();
                self.consume(TK::Colon, "Expect ':' after 'case' value.");
                self.emit_byte(OpCode::Equal, self.line());
                let jump = self.emit_jump(OpCode::JumpIfFalse);
                self.emit_byte(OpCode::Pop, self.line()); // Get rid of true comparison
                Some(jump)
            } else {
                // The default case does not need to get jumped over
                self.consume(TK::Default, "Expect 'case' or 'default'.");
                self.consume(TK::Colon, "Expect ':' after 'default'");
                had_default = true;
                None
            };

            // Read the statements belonging to the current case
            while !self.check(TK::RightBrace)
                && !self.check(TK::Case)
                && !self.check(TK::Default)
                && !self.check(TK::Eof)
            {
                self.statement();
            }

            // If a branch has been entered then after it is finished we jump all the
            // way to the end of the switch
            end_jumps.push(self.emit_jump(OpCode::Jump));

            if let Some(miss_jump) = miss_jump {
                self.patch_jump(miss_jump);
                // Get rid of the 'false' of the comparison
                self.emit_byte(OpCode::Pop, self.line());
            }
        }

        for end_jump in end_jumps {
            self.patch_jump(end_jump);
        }

        self.emit_byte(OpCode::Pop, self.line()); // Get rid of switch value

        self.consume(TK::RightBrace, "Expect '}' after 'switch' body.");
    }
}
