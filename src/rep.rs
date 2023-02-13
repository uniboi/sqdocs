	use sqparse::{
		ast::{Expression, FunctionArg, FunctionArgs, Statement, StatementType, TableSlot, Type},
		parse, tokenize, Flavor,
	};

	fn get_lead(depth: usize) -> String {
		"  ".repeat(depth) // TODO: read from config
	}
	
	fn get_expression_type(exp: &Expression) -> String {
		String::from(match exp {
			Expression::Literal(exp) => match exp.literal {
				sqparse::token::LiteralToken::Int(_, _) => "int",
				sqparse::token::LiteralToken::Char(_) => "int",
				sqparse::token::LiteralToken::Float(_) => "float",
				sqparse::token::LiteralToken::String(s) => match s {
					sqparse::token::StringToken::Asset(_) => "asset",
					_ => "string",
				},
			},
			Expression::Table(_) => "table", // TODO: content types derived from expression
			Expression::Array(_) => "array", // TODO: content typed derived from expression
			Expression::Vector(_) => "vector",
			_ => "var",
		})
	}
	
	fn is_empty_block(stm: &Statement) -> bool {
		match &stm.ty {
			StatementType::Block(b) => b.statements.len() == 0,
			_ => panic!("not a block statement"),
		}
	}
	
	fn get_statement_rep(statement: &Statement, depth: usize, start_inline: bool) -> String {
		let mut leading: String = String::from("\n");
		let mut trailing_semicolon = true; // TODO: read from config
		let mut use_lead: bool = true;
	
		let rep = match &statement.ty {
			StatementType::Empty(_) => String::from("<empty>"), // TODO: idk
			StatementType::Block(e) => {
				trailing_semicolon = false;
				use_lead = false;
				let (rep, lead) = get_block_rep(e, depth);
				leading = lead;
				rep
			}
			StatementType::If(stm) => {
				trailing_semicolon = false;
				get_if_rep(stm, depth)
			}
			StatementType::While(stm) => {
				trailing_semicolon = false;
				get_while_rep(stm, depth)
			}
			StatementType::DoWhile(stm) => {
				trailing_semicolon = false;
				get_do_while_rep(stm, depth)
			}
			StatementType::Switch(stm) => {
				trailing_semicolon = false;
				get_switch_rep(stm, depth)
			}
			StatementType::For(_) => todo!(),
			StatementType::ForeachStatement(f) => {
				trailing_semicolon = false;
				get_foreach_rep(f, depth)
			}
			StatementType::Break(_) => format!("break"),
			StatementType::Continue(_) => format!("continue"),
			StatementType::Return(e) => format!(
				"return{}",
				match &e.value {
					Some(exp) => format!(" {}", get_expression_rep(exp, depth)),
					None => String::new(),
				}
			),
			StatementType::Yield(y) => format!(
				"yield{}",
				match &y.value {
					Some(y) => format!(" {}", get_expression_rep(y, depth)),
					None => String::new(),
				}
			),
			StatementType::VarDeclaration(_) => todo!(),
			StatementType::ConstructorDeclaration(_) => todo!(),
			StatementType::FunctionDeclaration(e) => {
				trailing_semicolon = false;
				format!(
					"{} function {}({}){}{}",
					get_type_rep(&e.return_type, depth),
					e.name.last_item.value,
					get_function_param_rep(&e.declaration.args, depth),
					match &e.declaration.captures {
						Some(capture) => get_capture_rep(capture),
						None => "".to_owned(),
					},
					get_statement_rep(&e.declaration.body, depth + 1, false)
				)
			}
			StatementType::ClassDeclaration(_) => todo!(),
			StatementType::TryCatch(t) => {
				trailing_semicolon = false;
				get_try_catch_rep(&t, depth)
			}
			StatementType::Throw(t) => format!("throw {}", get_expression_rep(&t.value, depth)),
			StatementType::Const(c) => get_const_rep(c, depth),
			StatementType::Enum(_) => todo!(),
			StatementType::Expression(e) => {
				format!("{}", get_expression_rep(&e.value, depth))
			}
			StatementType::Thread(t) => format!("thread {}", get_expression_rep(&*t.value, depth)),
			StatementType::DelayThread(d) => get_delaythread_rep(&d, depth),
			StatementType::WaitThread(w) => {
				format!("waitthread {}", get_expression_rep(&*w.value, depth))
			}
			StatementType::Wait(w) => format!("wait {}", get_expression_rep(&*w.value, depth)),
			StatementType::StructDeclaration(_) => todo!(),
			StatementType::Typedef(d) => format!(
				"typedef {} {}",
				d.name.value,
				get_typed_type_rep(&d.ty, depth)
			),
			StatementType::Global(_) => todo!(),
			StatementType::GlobalizeAllFunctions(_) => {
				leading = String::from("");
				String::from("globalize_all_functions")
			}
			StatementType::Untyped(_) => {
				leading = String::from("");
				String::from("untyped")
			}
		};
	
		let lead = if start_inline { format!("") } else { leading };
		let pre_whitespace = if use_lead && !start_inline {
			get_lead(depth)
		} else {
			format!("")
		};
		let semicolon = if trailing_semicolon { ";" } else { "" };
	
		let p = format!("{}{}{}{}", lead, pre_whitespace, rep, semicolon,);
		p
	}
	
	// fn get_for_rep(stm: &sqparse::ast::ForStatement, depth: usize) -> String {
	//     let pre_pad = ""; // TODO: read from config
	//     let in_pad = " "; // TODO: read from config
	//     let pre_body_pad = "\n"; // TODO: read from config
	//     format!(
	//         "for{pre_pad}({in_pad}{};;{in_pad})",
	//         match &stm.initializer {
	//             Some(init) => format!("{}", get_expression_rep(&init, depth)),
	//             None => format!(""),
	//         }
	//     )
	// }
	
	fn get_foreach_rep(stm: &sqparse::ast::ForeachStatement, depth: usize) -> String {
		let pre_pad = ""; // TODO: read from config
		let in_pad = " "; // TODO: read from config
		format!(
			"foreach{pre_pad}({in_pad}{}{}{} in {}{in_pad}){}",
			match &stm.index {
				Some(idx) => format!("{} {}, ", get_type_rep(&idx.ty, depth), idx.name.value),
				None => format!(""),
			},
			match &stm.value_type {
				Some(ty) => format!("{} ", get_typed_type_rep(&ty, depth)),
				None => format!(""), // TODO: infer from iterator somehow
			},
			stm.value_name.value,
			get_expression_rep(&*stm.array, depth),
			get_statement_rep(&*stm.body, depth + 1, is_empty_block(&*stm.body))
		)
	}
	
	fn get_switch_rep(stm: &sqparse::ast::SwitchStatement, depth: usize) -> String {
		let pre_pad = ""; // TODO: read from config
		let in_pad = " "; // TODO: read from config
		let pre_body_pad = "\n"; // TODO: read from config
		format!(
			"switch{pre_pad}({in_pad}{}{in_pad}){pre_body_pad}{}{{{}\n{}}}",
			get_expression_rep(&*stm.condition, depth),
			get_lead(depth),
			stm.cases
				.iter()
				.map(|case| format!(
					"\n{}{}",
					get_lead(depth + 1),
					get_switch_case_rep(case, depth + 2)
				))
				.collect::<Vec<_>>()
				.join(&format!("")),
			get_lead(depth)
		)
	}
	
	fn get_switch_case_rep(stm: &sqparse::ast::SwitchCase, depth: usize) -> String {
		match &stm.condition {
			sqparse::ast::SwitchCaseCondition::Default { default: _ } => {
				format!(
					"default:{}",
					stm.body
						.iter()
						.map(|st| get_statement_rep(st, depth, false))
						.collect::<Vec<_>>()
						.join("")
				)
			}
			sqparse::ast::SwitchCaseCondition::Case { case: _, value } => format!(
				"case {}:{}",
				get_expression_rep(&*value, depth),
				stm.body
					.iter()
					.map(|st| get_statement_rep(st, depth, false))
					.collect::<Vec<_>>()
					.join("")
			),
		}
	}
	
	fn get_if_rep(stm: &sqparse::ast::IfStatement, depth: usize) -> String {
		let pre_cond_pad = ""; // TODO: read from config
		let in_cond_pad = " "; // TODO: read from config
		let block_new_line = true; // TODO: read from config
		let inline_block_pre = " "; // TODO: read from config
	
		format!(
			"if{pre_cond_pad}({in_cond_pad}{}{in_cond_pad}){}{}{}",
			get_expression_rep(&*stm.condition, depth),
			if block_new_line {
				format!("")
			} else {
				// TODO: make this work
				format!("{inline_block_pre}")
			},
			get_statement_rep(&*stm.body, depth + 1, false),
			match &stm.else_ {
				Some(el) => {
					let elif = matches!(&el.body.ty, StatementType::If(_));
					format!(
						"\n{}else {}",
						get_lead(depth),
						get_statement_rep(&*el.body, depth + if elif { 0 } else { 1 }, elif)
					)
				}
				None => format!(""),
			}
		)
	}
	
	fn get_while_rep(stm: &sqparse::ast::WhileStatement, depth: usize) -> String {
		let pre_cond_pad = ""; // TODO: read from config
		let in_cond_pad = " "; // TODO: read from config
		let block_new_line = false; // TODO: read from config
		let inline_block_pre = " "; // TODO: read from config
	
		format!(
			"while{pre_cond_pad}({in_cond_pad}{}{in_cond_pad}){}{}",
			get_expression_rep(&*stm.condition, depth),
			if block_new_line {
				format!("")
			} else {
				// TODO: make this work
				format!("{inline_block_pre}")
			},
			get_statement_rep(&*stm.body, depth + 1, false)
		)
	}
	
	fn get_do_while_rep(stm: &sqparse::ast::DoWhileStatement, depth: usize) -> String {
		// TODO: config stuff
		let cond_pad = " "; // TODO: read from config
		format!(
			"do{}{} while({cond_pad}{}{cond_pad})",
			if is_empty_block(&*stm.body) {
				" "
			} else {
				"\n"
			},
			get_statement_rep(&*stm.body, depth + 1, true),
			get_expression_rep(&*stm.condition, depth)
		)
	}
	
	fn get_const_rep(exp: &sqparse::ast::ConstStatement, depth: usize) -> String {
		// TODO: infer more types from initializer
		let infer_types = false; // TODO: read from config
		format!(
			"const {}{} = {}",
			if infer_types {
				format!(
					"{} ",
					match &exp.const_type {
						Some(ty) => get_typed_type_rep(&ty, depth),
						None => get_expression_type(&exp.initializer.value),
					}
				)
			} else {
				String::from("")
			},
			exp.name.value,
			get_expression_rep(&exp.initializer.value, depth)
		)
	}
	
	fn get_try_catch_rep(exp: &sqparse::ast::TryCatchStatement, depth: usize) -> String {
		let padding = " "; // TODO: read from config
		let lead = get_lead(depth);
		format!(
			"{}try{}\n{}catch({padding}{}{padding}){}",
			lead,
			get_statement_rep(&*exp.body, depth + 1, false),
			lead,
			exp.catch_name.value,
			get_statement_rep(&*exp.catch_body, depth + 1, false)
		)
	}
	
	fn get_delaythread_rep(expr: &sqparse::ast::DelayThreadStatement, depth: usize) -> String {
		let padding = " "; // TODO: read from config
		let pre = ""; // TODO: read from config
		format!(
			"delaythread{pre}({padding}{}{padding}) {}",
			get_expression_rep(&*expr.duration, depth),
			get_expression_rep(&*expr.value, depth)
		)
	}
	
	pub fn get_type_rep(ty: &Option<Type>, depth: usize) -> String {
		match ty {
			Some(ty) => get_typed_type_rep(ty, depth),
			None => String::from("var"),
		}
	}
	
	fn get_boxed_type_rep(ty: &Option<Box<sqparse::ast::Type<'_>>>, depth: usize) -> String {
		match ty {
			Some(ty) => get_typed_type_rep(ty, depth),
			None => String::from("var"),
		}
	}
	
	pub fn get_typed_type_rep(ty: &Type, depth: usize) -> String {
		match &ty {
			Type::Local(_) => String::from("local"),
			Type::Var(_) => String::from("var"),
			Type::Plain(t) => String::from(t.name.value),
			Type::Array(t) => format!(
				"{}[{}]",
				get_typed_type_rep(&*t.base, depth),
				get_expression_rep(&t.len, depth)
			),
			Type::Generic(g) => get_generic_type_rep(g, depth),
			Type::FunctionRef(f) => get_functionref_type_rep(f, depth),
			Type::Struct(_) => todo!(),
			Type::Reference(r) => format!("{}&", get_typed_type_rep(&*r.base, depth)),
			Type::Nullable(n) => format!("{} ornull", get_typed_type_rep(&*n.base, depth)),
		}
	}
	
	fn get_functionref_type_rep(f: &sqparse::ast::FunctionRefType, depth: usize) -> String {
		let padding = " "; // TODO: read from config
		let args_rep = get_functionref_args_rep(&f.args, depth);
		format!(
			"{} functionref({padding}{args_rep}{padding})",
			get_boxed_type_rep(&f.return_type, depth),
		)
	}
	
	fn get_functionref_args_rep(
		args: &sqparse::ast::SeparatedListTrailing0<sqparse::ast::FunctionRefArg>,
		depth: usize,
	) -> String {
		format!(
			"{}",
			match args {
				Some(args) => format!(
					"{}{}{}",
					args.items
						.iter()
						.map(|(arg, _)| get_functionref_arg_rep(&arg, depth))
						.collect::<Vec<_>>()
						.join(", "),
					if args.items.len() > 0 { ", " } else { "" },
					get_functionref_arg_rep(&args.last_item, depth),
				),
				None => String::from(""),
			}
		)
	}
	
	fn get_functionref_arg_rep(arg: &sqparse::ast::FunctionRefArg, depth: usize) -> String {
		format!(
			"{}{}",
			get_typed_type_rep(&arg.ty, depth),
			match &arg.name {
				Some(name) => format!(" {}", name.value),
				None => String::from(""),
			}
		)
	}
	
	fn get_generic_type_rep(ty: &sqparse::ast::GenericType, depth: usize) -> String {
		format!(
			"{}<{}>",
			get_typed_type_rep(&*ty.base, depth),
			get_generic_type_content_rep(&ty.params, depth)
		)
	}
	
	fn get_generic_type_content_rep(
		types: &sqparse::ast::SeparatedListTrailing1<Type>,
		depth: usize,
	) -> String {
		let mut padding = ""; // TODO: Read from config
		let rep = format!(
			"{}{}{}",
			types
				.items
				.iter()
				.map(|(t, _)| get_typed_type_rep(t, depth))
				.collect::<Vec<_>>()
				.join(", "),
			if types.items.len() > 0 { ", " } else { "" },
			get_typed_type_rep(&*types.last_item, depth)
		);
	
		if matches!(&*types.last_item, Type::Generic(_)) {
			padding = " "; // This is required to compile since right bit shift (>>) will be lexed before any types
		}
	
		format!("{padding}{}{padding}", rep)
	}
	
	pub fn get_function_param_rep(args: &FunctionArgs, depth: usize) -> String {
		match args {
			FunctionArgs::NonVariable { args } => match args {
				Some(args) => get_all_typed_args_rep(&args.items, &args.last_item, depth),
				None => "".to_owned(),
			},
			FunctionArgs::EmptyVariable { vararg: _ } => " ... ".to_owned(),
			FunctionArgs::NonEmptyVariable {
				args,
				comma: _,
				vararg: _,
			} => format!(
				"{}, ... ",
				get_all_typed_args_rep(&args.items, &args.last_item, depth)
			),
		}
	}
	
	fn get_typed_arg_rep(arg: &FunctionArg, depth: usize) -> String {
		format!(
			"{} {}{}",
			get_type_rep(&arg.ty, depth),
			arg.name.value,
			match &arg.initializer {
				Some(init) => format!(" = {}", get_expression_rep(&*init.value, depth)),
				None => String::from(""),
			}
		)
	}
	
	fn get_all_typed_args_rep(
		args: &Vec<(FunctionArg<'_>, &sqparse::token::Token<'_>)>,
		last_arg: &FunctionArg,
		depth: usize,
	) -> String {
		format!(
			" {}{}{} ",
			args.iter()
				.map(|(arg, _)| get_typed_arg_rep(arg, depth))
				.collect::<Vec<_>>()
				.join(", "),
			if args.len() > 0 { ", " } else { "" },
			get_typed_arg_rep(last_arg, depth)
		)
	}
	
	fn get_capture_rep(capture: &sqparse::ast::FunctionCaptures) -> String {
		format!(
			" : ({})",
			match &capture.names {
				Some(idens) => format!(
					" {}{}{} ",
					idens
						.items
						.iter()
						.map(|(identifier, _)| identifier.value)
						.collect::<Vec<_>>()
						.join(", "),
					if idens.items.len() > 0 { ", " } else { " " },
					idens.last_item.value
				),
				None => String::new(),
			}
		)
	}
	
	pub fn get_expression_rep(expression: &Expression, depth: usize) -> String {
		format!(
			"{}",
			match &expression {
				Expression::Parens(p) => get_parens_rep(p, depth),
				Expression::Literal(exp) => get_literal_rep(exp),
				Expression::Var(var) => format!("{}", var.name.value),
				Expression::RootVar(var) => format!("::{}", var.name.value),
				Expression::Index(exp) => get_index_rep(&exp, depth),
				Expression::Property(p) => get_property_rep(p, depth),
				Expression::Ternary(t) => get_ternary_rep(t, depth),
				Expression::Binary(b) => get_binary_rep(b, depth),
				Expression::Prefix(p) => format!(
					"{}{}",
					get_prefix_rep(&p.operator),
					get_expression_rep(&p.value, depth)
				),
				Expression::Postfix(p) => format!(
					"{}{}",
					get_expression_rep(&p.value, depth),
					get_postfix_rep(&p.operator)
				),
				Expression::Comma(_) => todo!(),
				Expression::Table(table) => get_table_rep(table, depth),
				Expression::Class(_) => todo!(),
				Expression::Array(arr) => get_array_rep(arr, depth),
				Expression::Function(exp) => format!(
					"{} function({}){}{}",
					get_type_rep(&exp.return_type, depth),
					get_function_param_rep(&exp.declaration.args, depth),
					match &exp.declaration.captures {
						Some(capture) => get_capture_rep(capture),
						None => "".to_owned(),
					},
					get_statement_rep(&exp.declaration.body, depth + 1, false)
				),
				Expression::Call(c) => get_call_rep(c, depth),
				Expression::Delegate(d) => format!(
					"delegate {} : {}",
					get_expression_rep(&*d.parent, depth),
					get_expression_rep(&*d.value, depth)
				),
				Expression::Vector(v) => get_vector_rep(&v, depth),
				Expression::Expect(ex) => {
					let padding = " "; // TODO: read from config
					format!(
						"expect {}({padding}{}{padding})",
						get_typed_type_rep(&ex.ty, depth),
						get_expression_rep(&*ex.value, depth)
					)
				}
			}
		)
	}
	
	fn get_array_rep(exp: &sqparse::ast::ArrayExpression, depth: usize) -> String {
		let padding = " "; // TODO: read from config
		let max_oneliner_items = 5; // TODO: read from config
		format!(
			"[{}]",
			if exp.values.len() <= max_oneliner_items {
				format!("{padding}{}{padding}", get_array_oneliner_rep(exp, depth))
			} else {
				format!(
					"\n{}{}\n{}",
					get_lead(depth + 1),
					get_array_multiliner_rep(exp, depth + 1),
					get_lead(depth)
				)
			}
		)
	}
	
	fn get_array_oneliner_rep(exp: &sqparse::ast::ArrayExpression, depth: usize) -> String {
		let spread = "...";
		let rep = exp
			.values
			.iter()
			.map(|v| get_expression_rep(&*v.value, depth))
			.collect::<Vec<_>>()
			.join(", ");
		format!(
			"{rep}{}",
			match exp.spread {
				Some(_) =>
					if exp.values.len() > 0 {
						format!(", {spread}")
					} else {
						format!("{spread}")
					},
				None => String::from(""),
			}
		)
	}
	
	fn get_array_multiliner_rep(exp: &sqparse::ast::ArrayExpression, depth: usize) -> String {
		let rep = exp.values
			.iter()
			.map(|v| get_expression_rep(&*v.value, depth))
			.collect::<Vec<_>>()
			.join(&format!(",\n{}", get_lead(depth)));
		format!("{rep}{}", match exp.spread {
			Some(_) => format!(",\n{}...", get_lead(depth)),
			None => String::from("")
		})
	}
	
	fn get_prefix_rep(op: &sqparse::ast::PrefixOperator) -> String {
		String::from(match op {
			sqparse::ast::PrefixOperator::Negate(_) => "-",
			sqparse::ast::PrefixOperator::LogicalNot(_) => "!",
			sqparse::ast::PrefixOperator::BitwiseNot(_) => "~",
			sqparse::ast::PrefixOperator::Typeof(_) => "typeof",
			sqparse::ast::PrefixOperator::Clone(_) => "clone",
			sqparse::ast::PrefixOperator::Delete(_) => "delete",
			sqparse::ast::PrefixOperator::Increment(_) => "++",
			sqparse::ast::PrefixOperator::Decrement(_) => "--",
		})
	}
	
	fn get_postfix_rep(op: &sqparse::ast::PostfixOperator) -> String {
		String::from(match op {
			sqparse::ast::PostfixOperator::Increment(_) => "++",
			sqparse::ast::PostfixOperator::Decrement(_) => "--",
		})
	}
	
	fn get_ternary_rep(exp: &sqparse::ast::TernaryExpression, depth: usize) -> String {
		format!(
			"{} ? {} : {}",
			get_expression_rep(&*exp.condition, depth),
			get_expression_rep(&*exp.true_value, depth),
			get_expression_rep(&*exp.false_value, depth)
		)
	}
	
	fn get_index_rep(exp: &sqparse::ast::IndexExpression, depth: usize) -> String {
		let padding = " "; // TODO: read from config
		format!(
			"{}[{padding}{}{padding}]",
			get_expression_rep(&*exp.base, depth),
			get_expression_rep(&*exp.index, depth)
		)
	}
	
	fn get_property_rep(exp: &sqparse::ast::PropertyExpression, depth: usize) -> String {
		format!(
			"{}.{}",
			get_expression_rep(&*exp.base, depth),
			get_method_identifier_rep(&exp.property)
		)
	}
	
	fn get_method_identifier_rep(exp: &sqparse::ast::MethodIdentifier) -> String {
		String::from(match exp {
			sqparse::ast::MethodIdentifier::Identifier(exp) => exp.value,
			sqparse::ast::MethodIdentifier::Constructor(_) => "constructor",
		})
	}
	
	fn get_vector_rep(exp: &sqparse::ast::VectorExpression, depth: usize) -> String {
		let padding = " "; // TODO: read from config
		format!(
			"<{padding}{}, {}, {}{padding}>",
			get_expression_rep(&exp.x, depth),
			get_expression_rep(&exp.y, depth),
			get_expression_rep(&exp.z, depth)
		)
	}
	
	fn get_parens_rep(exp: &sqparse::ast::ParensExpression, depth: usize) -> String {
		let padding = " "; // TODO: read from config
		format!(
			"({padding}{}{padding})",
			get_expression_rep(&*exp.value, depth)
		)
	}
	
	fn get_binary_rep(exp: &sqparse::ast::BinaryExpression, depth: usize) -> String {
		format!(
			"{} {} {}",
			get_expression_rep(&*exp.left, depth),
			get_binary_op_rep(&exp.operator),
			get_expression_rep(&*exp.right, depth)
		)
	}
	
	fn get_binary_op_rep(op: &sqparse::ast::BinaryOperator) -> String {
		String::from(match op {
			sqparse::ast::BinaryOperator::Assign(_) => "=",
			sqparse::ast::BinaryOperator::AssignNewSlot(_, _) => "<-",
			sqparse::ast::BinaryOperator::AssignAdd(_) => "+=",
			sqparse::ast::BinaryOperator::AssignSubtract(_) => "-=",
			sqparse::ast::BinaryOperator::AssignMultiply(_) => "*=",
			sqparse::ast::BinaryOperator::AssignDivide(_) => "/=",
			sqparse::ast::BinaryOperator::AssignModulo(_) => "%=",
			sqparse::ast::BinaryOperator::Add(_) => "+",
			sqparse::ast::BinaryOperator::Subtract(_) => "-",
			sqparse::ast::BinaryOperator::Multiply(_) => "*",
			sqparse::ast::BinaryOperator::Divide(_) => "/",
			sqparse::ast::BinaryOperator::Modulo(_) => "%",
			sqparse::ast::BinaryOperator::Equal(_) => "==",
			sqparse::ast::BinaryOperator::NotEqual(_) => "!=",
			sqparse::ast::BinaryOperator::Less(_) => "<",
			sqparse::ast::BinaryOperator::LessEqual(_) => "<=",
			sqparse::ast::BinaryOperator::Greater(_) => ">",
			sqparse::ast::BinaryOperator::GreaterEqual(_) => ">=",
			sqparse::ast::BinaryOperator::ThreeWay(_) => "<=>",
			sqparse::ast::BinaryOperator::LogicalAnd(_) => "&&",
			sqparse::ast::BinaryOperator::LogicalOr(_) => "||",
			sqparse::ast::BinaryOperator::BitwiseAnd(_) => "&",
			sqparse::ast::BinaryOperator::BitwiseOr(_) => "|",
			sqparse::ast::BinaryOperator::BitwiseXor(_) => "^",
			sqparse::ast::BinaryOperator::ShiftLeft(_, _) => "<<",
			sqparse::ast::BinaryOperator::ShiftRight(_, _) => ">>",
			sqparse::ast::BinaryOperator::UnsignedShiftRight(_, _, _) => ">>>",
			sqparse::ast::BinaryOperator::In(_) => "in",
			sqparse::ast::BinaryOperator::Instanceof(_) => "instanceof",
		})
	}
	
	fn get_literal_rep(exp: &sqparse::ast::LiteralExpression) -> String {
		match &exp.literal {
			sqparse::token::LiteralToken::Int(v, base) => get_integer_rep(exp, v, base),
			sqparse::token::LiteralToken::Char(c) => format!("'{c}'"),
			sqparse::token::LiteralToken::Float(f) => {
				let start_at_dot = false; // TODO: read from config
				let rep = format!("{f}");
	
				if start_at_dot {
					rep[if &exp.token.range.end - &exp.token.range.start < rep.len() {
						1
					} else {
						0
					} as usize..]
						.to_owned()
				} else {
					rep
				}
			}
			sqparse::token::LiteralToken::String(s) => get_string_rep(s),
		}
	}
	
	fn get_integer_rep(
		exp: &sqparse::ast::LiteralExpression,
		value: &i64,
		base: &sqparse::token::LiteralBase,
	) -> String {
		match base {
			sqparse::token::LiteralBase::Decimal => format!("{value}"),
			sqparse::token::LiteralBase::Octal => {
				let oct = format!("{value:o}");
				let preamble = "0".repeat(&exp.token.range.end - &exp.token.range.start - oct.len());
				format!("{preamble}{oct}")
			}
			sqparse::token::LiteralBase::Hexadecimal => {
				let prefix = "0x";
				let hex = format!("{value:X}");
				let preamble = "0"
					.repeat(&exp.token.range.end - &exp.token.range.start - hex.len() - prefix.len());
				format!("{prefix}{preamble}{hex}")
			}
		}
	}
	
	fn get_string_rep(exp: &sqparse::token::StringToken) -> String {
		match exp {
			sqparse::token::StringToken::Literal(s) => format!("\"{s}\""),
			sqparse::token::StringToken::Verbatim(v) => format!("@\"{v}\""),
			sqparse::token::StringToken::Asset(a) => format!("$\"{a}\""),
		}
	}
	
	fn get_table_rep(table: &sqparse::ast::TableExpression, depth: usize) -> String {
		let prop_inset = get_lead(depth + 1);
		let sep = format!(",\n{prop_inset}");
		let max_oneliner_items = 3; // TODO: read from config
	
		if table.slots.len() == 0 {
			return format!("{{}}");
		}
	
		if table.slots.len() > max_oneliner_items {
			format!(
				"{{{}\n{}{}\n{}}}",
				prop_inset,
				prop_inset,
				table
					.slots
					.iter()
					.map(|slot| get_table_pair(slot, depth))
					.collect::<Vec<_>>()
					.join(&sep),
				get_lead(depth)
			)
		} else {
			format!(
				"{{ {} }}",
				table
					.slots
					.iter()
					.map(|slot| get_table_pair(slot, depth))
					.collect::<Vec<_>>()
					.join(", ")
			)
		}
	}
	
	fn get_table_pair(s: &TableSlot, depth: usize) -> String {
		format!(
			"{}",
			match &s.ty {
				sqparse::ast::TableSlotType::Property { name, initializer } => format!(
					"{} = {}",
					name.value,
					get_expression_rep(&initializer.value, depth + 1)
				),
				sqparse::ast::TableSlotType::ComputedProperty {
					open: _,
					name,
					close: _,
					initializer,
				} => format!(
					"[{}] = {}",
					get_expression_rep(&name, depth + 1),
					get_expression_rep(&initializer.value, depth + 1)
				),
				sqparse::ast::TableSlotType::JsonProperty {
					name,
					colon: _,
					value,
				} => format!(
					"{} = {}",
					get_literal_rep(&name),
					get_expression_rep(&*value, depth)
				),
				sqparse::ast::TableSlotType::Function {
					return_type,
					function,
					name,
					declaration,
				} => todo!(),
			}
		)
	}
	
	fn get_call_rep(call: &sqparse::ast::CallExpression, depth: usize) -> String {
		format!(
			"{}({})",
			get_expression_rep(&call.function, depth),
			get_call_parameters_rep(&call.arguments, depth)
		)
	}
	
	fn get_call_parameters_rep(
		args: &Option<sqparse::ast::SeparatedListTrailing1<Expression>>,
		depth: usize,
	) -> String {
		match args {
			Some(list) => format!(
				" {}{}{} ",
				list.items
					.iter()
					.map(|(expression, _)| get_expression_rep(expression, depth))
					.collect::<Vec<_>>()
					.join(", "),
				if list.items.len() > 0 { ", " } else { "" },
				get_expression_rep(&list.last_item, depth)
			),
			None => String::from(""),
		}
	}
	
	fn get_block_rep(block: &sqparse::ast::BlockStatement, depth: usize) -> (String, String) {
		if block.statements.len() == 0 {
			return (format!("{{}}"), String::from(" "));
		}
		(
			format!(
				"{}{{{}\n{}}}",
				get_lead(depth - 1),
				block
					.statements
					.iter()
					.map(|statement| get_statement_rep(statement, depth, false))
					.collect::<Vec<_>>()
					.join(" "),
				get_lead(depth - 1)
			),
			String::from("\n"),
		)
	}