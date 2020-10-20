#include <iostream>
#include <string>
#include <vector>
#include <array>
#include <variant>
#include <assert.h>
#include <fstream>
#include <unordered_map>
#include <charconv>
#include <functional>
#include <cmath>
#include <stack>

#include <spdlog/spdlog.h>



struct CAS
{
	using number = long double;
	using name = std::string_view;

	enum class op { pow, sign_neg, sign_pos, mul, div, add, sub, com };
	enum class sign { neg, pos };
	enum class bracket { left, right };
	using symbol = std::variant<number, name>;
	using item = std::variant<symbol, op, bracket, sign, std::monostate>;
	using expr = std::vector<item>;
	constexpr static std::string_view operators = "^$$*/+-,";
	constexpr static std::string_view signs = "-+";
	constexpr static std::string_view brackets = "()";

	// functions labeled: functions_<num parameters>
	// constants are 0 parameter functions

	template<size_t N>
	using function_map = std::unordered_map<name, std::function<number(const std::array<number, N>&)>>;
	using operator_map = std::unordered_map<op, std::function<number(const std::array<number, 2>&)>>;
	using operator_order_map = std::unordered_map<op, int32_t>;

	function_map<0> functions_0 = {
		{ "pi", [](const std::array<number, 0>& in){ return 3.1415926535897932384626433832795; } },
		{ "e",  [](const std::array<number, 0>& in){ return 2.7182818284590452353602874713527; } },
	};

	function_map<1> functions_1 = {
		{ "sin", [](const std::array<number, 1>& in){ return std::sinl(in.at(0)); } },
		{ "cos", [](const std::array<number, 1>& in){ return std::cosl(in.at(0)); } },
		{ "tan", [](const std::array<number, 1>& in){ return std::tanl(in.at(0)); } },

		{ "asin", [](const std::array<number, 1>& in){ return std::asinl(in.at(0)); } },
		{ "acos", [](const std::array<number, 1>& in){ return std::acosl(in.at(0)); } },
		{ "atan", [](const std::array<number, 1>& in){ return std::atanl(in.at(0)); } },

		{ "ln", [](const std::array<number, 1>& in){ return std::logl(in.at(0)); } },
	};

	function_map<2> functions_2 = {
		{ "log", [](const std::array<number, 2>& in){ return std::logl(in.at(1)) / std::logl(in.at(0)); } },
	};

	operator_map operator_functions = {
		{ op{ 0 }, [](const std::array<number, 2>& in){ return std::powl(in.at(0), in.at(1)); } },
		{ op{ 3 }, [](const std::array<number, 2>& in){ return in.at(0) * in.at(1); } },
		{ op{ 4 }, [](const std::array<number, 2>& in){ return in.at(0) / in.at(1); } },
		{ op{ 5 }, [](const std::array<number, 2>& in){ return in.at(0) + in.at(1); } },
		{ op{ 6 }, [](const std::array<number, 2>& in){ return in.at(0) - in.at(1); } },
	};

	operator_order_map operator_orders = {
		{ op{ 0 }, 0 },
		{ op{ 1 }, 1 },
		{ op{ 2 }, 1 },
		{ op{ 3 }, 2 },
		{ op{ 4 }, 2 },
		{ op{ 5 }, 3 },
		{ op{ 6 }, 3 },
	};



	bool is_number(const char& c)
	{
		return std::isdigit(c) || c == '.';
	}

	bool is_operator(const char& c)
	{
		return operators.find(c) != std::string_view::npos;
	}

	expr stack_from_input(const std::string& input)
	{
		expr output_stack;

		for (size_t i = 0; i < input.size(); i++)
		{
			auto lambda = [&](auto call){
				for (size_t j = i+1; j < input.size() + 1; j++)
					if (call(j))
						break;
			};

			// following is text
			if(std::isalpha(input[i])) // functions and variables
			{
				lambda([&](size_t j){
					if(std::isalpha(input[j]))
						return false;
						
					output_stack.push_back(static_cast<std::string_view>(input).substr(i, j-i));
					i = j-1;
					return true;
				});
			}
			else if(is_number(input[i])) // numbers
			{
				lambda([&](size_t j){
					if(is_number(input[j]))
						return false;
					
					number parsed_num = 0.0;
					std::string_view to_be_parsed = static_cast<std::string_view>(input).substr(i, j-i);
					try {
						parsed_num = std::stold(static_cast<std::string>(to_be_parsed));
					} catch(const std::exception&) {
						std::cerr << "could not parse: " << to_be_parsed;
						assert(0);
					}

					output_stack.push_back(parsed_num);
					i = j-1;
					return true;
				});
			}
			else if(is_operator(input[i])) // operators
			{
				size_t pos = operators.find(input[i]);
				if(pos == std::string_view::npos)
				{
					std::cerr << "Invalid operator: " << input[i];
					assert(0);
				}

				if(
					output_stack.empty() ||
					std::holds_alternative<op>(output_stack.back()) ||
					std::holds_alternative<sign>(output_stack.back()) ||
					(std::holds_alternative<bracket>(output_stack.back()) && std::get<bracket>(output_stack.back()) == bracket::left)
				)
					output_stack.push_back(input[i] == '-' ? sign::neg : sign::pos);
				else	
					output_stack.push_back(op{ static_cast<uint8_t>(pos) });
			}
			else if(brackets.find(input[i]) != std::string_view::npos)
			{
				output_stack.push_back(input[i] == '(' ? bracket::left : bracket::right);
			}
		}


		return output_stack;
	}



	void fix_stack(expr& stack)
	{
		/* for (size_t i = 0; i < stack.size(); i++)
		{
			auto val = std::get_if<std::string_view>(&stack[i]);
			if(!val) continue;

			function_map<0>::iterator iter_0;
			function_map<1>::iterator iter_1;
			function_map<2>::iterator iter_2;

			// if no ( or ) only use first stack value as input
			if([&]{ iter_0 = functions_0.find(*val); return iter_0 != functions_0.end(); }())
			{
				// pi -> pi()
				continue;
			}
			else if ([&]{ iter_1 = functions_1.find(*val); return iter_1 != functions_1.end(); }())
			{
				// cos x -> cos(x), cos(x) -> cos(x), cos() -> error
				//TODO:
			}
			else if ([&]{ iter_2 = functions_2.find(*val); return iter_2 != functions_2.end(); }())
			{
				// log x,y -> log(x,y), log(x,y) -> log(x,y), log(x) -> <error>
				//TODO:
			}
			else
			{
				std::cerr << "Unknown function: " << *val;
				assert(0);
			}
		} */
	}

	// Shunting Yard Algorithm
	void make_rpn(expr& input)
	{
		expr output;
		std::stack<item> stack;
		output.reserve(input.size());

		for(const auto& item : input)
		{
			std::visit([&output, &stack, this](const auto& value){
				using T = std::decay_t<decltype(value)>;

				// type symbol
				if constexpr(std::is_same_v<T, symbol>)
				{
					output.push_back(value);
				}

				// type operator or sign
				else if constexpr(std::is_same_v<T, op> || std::is_same_v<T, sign>)
				{
					op op_value;
					if constexpr(std::is_same_v<T, sign>)
						op_value = value == sign::neg ? op::sign_neg : op::sign_pos;
					else
						op_value = value;

					while(
						!stack.empty() &&
						std::holds_alternative<op>(stack.top()) &&
						( operator_orders.at( std::get<op>(stack.top()) ) <= operator_orders.at( op_value )) )
					{
						output.push_back(stack.top());
						stack.pop();
					}
					stack.push(value);
				}

				// type bracket
				else if constexpr(std::is_same_v<T, bracket>)
				{
					// left (
					if(value == bracket::left)
						stack.push(value);

					// right )
					else
					{
						while(!stack.empty() && !(std::holds_alternative<bracket>(stack.top()) && std::get<bracket>(stack.top()) != bracket::left))
						{
							output.push_back(stack.top());
							stack.pop();
						}
						if(!stack.empty())
							stack.pop();
						else
						{
							// empty
							// ignore the missing bracket and treat as if the last one is at the end
						}
					}
				}
			}, item);
		}
		while(!stack.empty())
		{
			output.push_back(stack.top());
			stack.pop();
		}
		input = output;
	}

	// ex.   5,5,-     =>   std::monostate,std::monostate,0
	// or    (x),5,-   =>   std::monostate,std::monostate,-5
	// or    5,x,-     =>   5,x,-
	void eval_rpn_part(expr& input, size_t op_is_at)
	{
		if(const op* as_op = std::get_if<op>(&input.at(op_is_at)))
		{
			auto a = std::find_if(input.rbegin() + input.size() - op_is_at, input.rend(), [](const auto& var){ return !std::holds_alternative<std::monostate>(var); });
			auto b = std::find_if(std::next(a), input.rend(), [](const auto& var){ return !std::holds_alternative<std::monostate>(var); });

			const symbol* as_symbol_a = a != input.crend() ? std::get_if<symbol>(&*a) : nullptr;
			const symbol* as_symbol_b = b != input.crend() ? std::get_if<symbol>(&*b) : nullptr;
			if(as_symbol_a == nullptr || as_symbol_b == nullptr)
				return; // 5*x - 4 or something
			
			const number* as_number_a = std::get_if<number>(as_symbol_a);
			const number* as_number_b = std::get_if<number>(as_symbol_b);
			if(as_number_a == nullptr || as_number_b == nullptr)
				return; // - x or - sin or something

			std::swap(as_number_a, as_number_b); // swapped around because they were searched in reverse
			input.at(op_is_at) = operator_functions.at(*as_op)({ *as_number_a, *as_number_b });
			*a = std::monostate{};
			*b = std::monostate{};
		}
		else if(const sign* as_sign = std::get_if<sign>(&input.at(op_is_at)))
		{
			auto a = std::find_if(input.rbegin() + input.size() - op_is_at, input.rend(), [](const auto& var){ return !std::holds_alternative<std::monostate>(var); });
			const symbol* as_symbol_a = a != input.crend() ? std::get_if<symbol>(&*a) : nullptr;
			if(as_symbol_a == nullptr)
				throw std::runtime_error("Non-Symbol item followed by a sign is not allowed");
			
			const number* as_number_a = std::get_if<number>(as_symbol_a);
			if(as_number_a == nullptr)
				return; // - x or - sin or something

			input.at(op_is_at) = *as_sign == sign::neg ? -*as_number_a : *as_number_a;
			*a = std::monostate{};
		}
	}

	// ex.   5,5,5,*,+   =>   std::monostate,std::monostate,std::monostate,std::monostate,30
	// or    5,-,5,-     =>   std::monostate,std::monostate,std::monostate,-10
	void eval_rpn(expr& input)
	{
		for(size_t i = 1; i < input.size(); i++)
		{
			eval_rpn_part(input, i);
			spdlog::debug(" - RPN eval {}: {}", i-1, input);
		}
	}

	void remove_monostate(expr& input)
	{
		input.erase(
			std::remove_if(input.begin(), input.end(), [](const item& i){ return std::get_if<std::monostate>(&i) != nullptr; }),
			input.end());
	}
};



template<>
struct fmt::formatter<CAS::symbol>
{
	template <typename ParseContext>
	constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

	template <typename FormatContext>
	auto format(const CAS::symbol& s, FormatContext& ctx) {
		auto out = ctx.out();
		auto visitor = [&ctx, &out](auto& val){
			using T = std::decay_t<decltype(val)>;
			out = fmt::formatter<T>{}.format(val, ctx);
			ctx.advance_to(out);
		};
		std::visit(visitor, s);
		return out;
	}
};

template<>
struct fmt::formatter<CAS::bracket>
{
	template <typename ParseContext>
	constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

	template <typename FormatContext>
	auto format(const CAS::bracket& bracket, FormatContext& ctx) {
		return format_to(ctx.out(), "{}", bracket == CAS::bracket::left ? '(' : ')');
	}
};

template<>
struct fmt::formatter<CAS::op>
{
	template <typename ParseContext>
	constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

	template <typename FormatContext>
	auto format(const CAS::op& op, FormatContext& ctx) {
		return format_to(ctx.out(), "{}", CAS::operators[static_cast<uint8_t>(op)]);
	}
};

template<>
struct fmt::formatter<CAS::sign>
{
	template <typename ParseContext>
	constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

	template <typename FormatContext>
	auto format(const CAS::sign& s, FormatContext& ctx) {
		return format_to(ctx.out(), "{}", s == CAS::sign::neg ? "neg" : "pos");
	}
};

template<>
struct fmt::formatter<std::monostate>
{
	template <typename ParseContext>
	constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

	template <typename FormatContext>
	auto format(const std::monostate& m, FormatContext& ctx) {
		return format_to(ctx.out(), "->");
	}
};

template<>
struct fmt::formatter<CAS::expr>
{
	template <typename ParseContext>
	constexpr auto parse(ParseContext& ctx) { return ctx.begin(); }

	template <typename FormatContext>
	auto format(const CAS::expr& obj, FormatContext& ctx) {
		constexpr std::string_view separator = ", ";
		auto out = ctx.out();
		auto visitor = [&ctx, &out](auto& val){
			using T = std::decay_t<decltype(val)>;
			out = fmt::formatter<T>{}.format(val, ctx);
			ctx.advance_to(out);
		};
		if(obj.size() == 0)
			return out;
		auto iter = obj.begin();

		// first one without comma
		if(iter != obj.end())
			std::visit(visitor, *iter);

		// last optional ones with comma
		for (iter = std::next(iter); iter != obj.end(); iter++)
		{
			out = std::copy(separator.begin(), separator.end(), out);
			ctx.advance_to(out);
			std::visit(visitor, *iter);
		}
		return out;
	}
};

int main(int argc, char** argv)
{
	spdlog::set_pattern("%^[%T] [%l]:%$ %v");
	spdlog::set_level(spdlog::level::level_enum::trace);
	// calculator instance
	CAS cas;

	for(size_t i = 0; i < argc; i++)
		spdlog::debug("arg{}: {}", i, argv[i]);

	if(argc == 4 && !std::strcmp(argv[1], "--ctest")) // ctest mode
	{
		spdlog::debug("Running ctest with args: '{}' & '{}'", argv[2], argv[3]);

		auto output_stack = cas.stack_from_input(argv[2]);
		cas.make_rpn(output_stack);
		cas.eval_rpn(output_stack);
		cas.remove_monostate(output_stack);

		const CAS::symbol* symbol = std::get_if<CAS::symbol>(&output_stack.at(0));
		if(symbol == nullptr)
			return 1;
		const CAS::number* number = std::get_if<CAS::number>(symbol);
		if(number == nullptr)
			return 2;
		
		double expected_output = std::stold(argv[3]);
		double actual_output = *number;
		spdlog::debug("Expected result: '{}', Actual result: '{}'", expected_output, actual_output);
		
		if(std::fabsl(expected_output - actual_output) > 0.000001)
			return 3;

		return 0;
	}
	else while(true)
	{
		std::cout << "Input:";
		std::string input;
		std::getline(std::cin, input);

		// gen stack from input
		spdlog::info("Input: {}", input);
		auto output_stack = cas.stack_from_input(input);
		spdlog::debug("Stack was: {}", output_stack);
		auto output_stack_rpn = output_stack;
		cas.make_rpn(output_stack_rpn);
		spdlog::debug("RPN stack was: {}", output_stack_rpn);
		auto evaluated_rpn = output_stack_rpn;
		cas.eval_rpn(evaluated_rpn);
		spdlog::debug("Evaluated RPN stack was: {}", evaluated_rpn);
		cas.remove_monostate(evaluated_rpn);
		spdlog::debug("Evaluated monostateless RPN stack was: {}", evaluated_rpn);
		spdlog::info("Answer: {}", evaluated_rpn);
		
		std::cout << "\n\n";
	}

	return 0;
}