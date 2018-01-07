#ifndef LBIZ_USERDEFINEDLITERALS_HPP
#define LBIZ_USERDEFINEDLITERALS_HPP

#include "BigInt.hpp"
#include "BigFixed.hpp"
#include "lfl/lfl.hpp"

namespace lbiz
{
	namespace detail
	{
		struct pred_e { constexpr bool operator()( char c ) const { return c == 'e' || c == 'E'; } };
		struct pred_floatingpoint { constexpr bool operator()( char c ) const { return c == '.'; } };

		struct char_to_underlying_type { constexpr underlying_type operator()( char c ) const { return c - '0'; } };
		struct pred_non_negative { constexpr underlying_type operator()( underlying_type i ) const { return i >= 0; } };
		


		template<char>
		constexpr bool hex_check() { return false; }
		template<char C0, char C1, char...>
		constexpr bool hex_check() { return C0 == '0' && ( C1 == 'x' || C1 == 'X' ); }

		template<char>
		constexpr bool bin_check() { return false; }
		template<char C0, char C1, char...>
		constexpr bool bin_check() { return C0 == '0' && ( C1 == 'b' || C1 == 'B' ); }

		template<char>
		constexpr bool oct_check() { return false; }
		template<char C0, char C1, char... Chars>
		constexpr bool oct_check() { return C0 == '0' && !hex_check<C0, C1, Chars...>() && !bin_check<C0, C1, Chars...>(); }

		template<std::size_t N>
		constexpr underlying_type concat_digits( const lfl::array<underlying_type, N>& arr, std::size_t begin, std::size_t end, underlying_type acc ) {
			return begin + 1 == end ? begin < N ? acc * 10 + arr[begin]
			                                    : acc * 10
				                    : begin < N ? concat_digits( arr, begin + 1, end, acc * 10 + arr[begin] )
					                            : concat_digits( arr, begin + 1, end, acc * 10 );
		}

		constexpr std::size_t BigInt_digits( std::size_t n ) {
			return ( n + BaseNumberOfDigits - 1 ) / BaseNumberOfDigits;
		}

		template<std::size_t N, lfl::index_t... Index>
		constexpr lbiz::BigInt<N> to_bigint( const lfl::array<underlying_type, N>& arr, lfl::index_tuple<Index...> ) {
			return BigInt<N> { MaxValueTag { Base - 1 }, MinValueTag { 0 }, arr[Index]... };
		}

	} // namespace detail

	namespace detail {
		struct pred_non_apostrophe { constexpr bool operator()( char c ) const { return c != '\''; } };

		template<std::size_t N, lfl::index_t... Index>
		constexpr lfl::array<underlying_type, N> to_concat_digits_array( const lfl::array<underlying_type, N>& arr, lfl::index_tuple<Index...> ) {
			return lfl::array<underlying_type, N> { { (
				Index % BaseNumberOfDigits == 0 ? concat_digits( arr, dot_sub( N - Index, BaseNumberOfDigits ), N - Index, 0 )
				                                : -1
			)... } };
		}

		// for cache
		template<char... Chars>
		struct BigIntLiteral
		{
			static constexpr lfl::array<char, sizeof...( Chars )> char_array { { Chars... } };

			static constexpr bool check =
				hex_check<Chars...>() ? throw "hex-literals are not supported" :
				bin_check<Chars...>() ? throw "binary-literals are not supported" :
				oct_check<Chars...>() ? throw "octal-literals are not suported" :
				lfl::any_of( pred_floatingpoint { }, char_array ) ? throw "floating-point literals are not supported (use _to_BigFixed)." :
				lfl::any_of( pred_e { }, char_array ) ? throw "exponent is not supported." :
				true;

			static constexpr std::size_t LiteralDigits = lfl::count_if( pred_non_apostrophe { }, char_array );

			static constexpr lfl::array<underlying_type, LiteralDigits> digits_array  =
				lfl::map( char_to_underlying_type { },
					lfl::filter<LiteralDigits>( pred_non_apostrophe { }, 
						char_array
					)
				);

			static constexpr lfl::array<underlying_type, LiteralDigits> concat_digits_array = to_concat_digits_array( digits_array, lfl::make_index_tuple<LiteralDigits> {} );

			static constexpr std::size_t N = BigInt_digits( LiteralDigits );

			static constexpr lfl::array<underlying_type, N> underlying_type_array = lfl::filter<N>( pred_non_negative { }, concat_digits_array );

			static constexpr BigInt<N> value = to_bigint( underlying_type_array, lfl::make_index_tuple<N> {} );
		};

		template<char... Chars>
		constexpr lfl::array<char, sizeof...( Chars )> BigIntLiteral<Chars...>::char_array;

		template<char... Chars>
		constexpr lfl::array<underlying_type, BigIntLiteral<Chars...>::LiteralDigits> BigIntLiteral<Chars...>::digits_array;

		template<char... Chars>
		constexpr lfl::array<underlying_type, BigIntLiteral<Chars...>::LiteralDigits> BigIntLiteral<Chars...>::concat_digits_array;

		template<char... Chars>
		constexpr BigInt<BigIntLiteral<Chars...>::N> BigIntLiteral<Chars...>::value;

	} // namespace detail

	template<char... Chars>
	constexpr BigInt<detail::BigIntLiteral<Chars...>::N> operator"" _to_BigInt() {
		return detail::BigIntLiteral<Chars...>::value;
	}

	namespace detail
	{
		struct pred_non_apostrophe_or_floatingpoint { constexpr bool operator()( char c ) const { return c != '\'' && c != '.'; } };

		struct count_fractional_part
		{
			constexpr std::size_t operator()( std::size_t n, char c ) const {
				return n == 0 ? c == '.' ? 1 : 0
				              : n + 1;
			}
		};

		template<std::size_t N, lfl::index_t... Index>
		constexpr lfl::array<underlying_type, N> to_concat_digits_array_fp( const lfl::array<underlying_type, N>& arr, std::size_t fp_digits, lfl::index_tuple<Index...> ) {
			return lfl::array<underlying_type, N> { { (
				Index <  fp_digits && ( Index + 1 ) % BaseNumberOfDigits == fp_digits % BaseNumberOfDigits ? concat_digits( arr, N - Index - 1, N - Index - 1 + BaseNumberOfDigits, 0 ) :
				Index >= fp_digits &&   Index       % BaseNumberOfDigits == fp_digits % BaseNumberOfDigits ? concat_digits( arr, dot_sub( N - Index, BaseNumberOfDigits ), N - Index, 0 )
				                                                                                           : -1
			)... } };
		}


		// for cache
		template<char... Chars>
		struct BigFixedLiteral
		{
			static constexpr lfl::array<char, sizeof...( Chars )> char_array { { Chars... } };

			static constexpr bool check =
				hex_check<Chars...>() ? throw "hex-literals are not supported" :
				bin_check<Chars...>() ? throw "binary-literals are not supported" :
				lfl::any_of( pred_e { }, char_array ) ? throw "exponent is not supported." :
				true;

			static constexpr std::size_t LiteralDigits = lfl::count_if( pred_non_apostrophe_or_floatingpoint { }, char_array );
			static constexpr std::size_t FractionalPartDigits = dot_sub( lfl::reduce( count_fractional_part { }, 0, char_array ), 1 );
			static constexpr std::size_t IntegerPartDigits = LiteralDigits - FractionalPartDigits;

			static constexpr std::size_t N = BigInt_digits( IntegerPartDigits );
			static constexpr std::size_t Q = BigInt_digits( FractionalPartDigits );

			static constexpr lfl::array<underlying_type, LiteralDigits> digits_array = 
				lfl::map( char_to_underlying_type { },
					lfl::filter<LiteralDigits>( pred_non_apostrophe_or_floatingpoint { },
						char_array
					)
				);

			static constexpr lfl::array<underlying_type, LiteralDigits> concat_digits_array = to_concat_digits_array_fp( digits_array, FractionalPartDigits, lfl::make_index_tuple<LiteralDigits> {} );

			static constexpr lfl::array<underlying_type, N + Q> underlying_type_array = lfl::filter<N + Q>( pred_non_negative { }, concat_digits_array );

			static constexpr BigFixed<N, Q> value { to_bigint( underlying_type_array, lfl::make_index_tuple<N + Q> {} ), ShiftTag<Q> {} };
		};

		template<char... Chars>
		constexpr lfl::array<char, sizeof...( Chars )>  BigFixedLiteral<Chars...>::char_array;

		template<char... Chars>
		constexpr lfl::array<underlying_type, BigFixedLiteral<Chars...>::LiteralDigits> BigFixedLiteral<Chars...>::digits_array;

		template<char... Chars>
		constexpr lfl::array<underlying_type, BigFixedLiteral<Chars...>::LiteralDigits> BigFixedLiteral<Chars...>::concat_digits_array;

		template<char... Chars>
		constexpr lfl::array<underlying_type, BigFixedLiteral<Chars...>::N + BigFixedLiteral<Chars...>::Q> BigFixedLiteral<Chars...>::underlying_type_array;

		template<char... Chars>
		constexpr BigFixed<BigFixedLiteral<Chars...>::N, BigFixedLiteral<Chars...>::Q> BigFixedLiteral<Chars...>::value;

	} // namespace detail

	template<char... Chars>
	constexpr BigFixed<detail::BigFixedLiteral<Chars...>::N, detail::BigFixedLiteral<Chars...>::Q> operator"" _to_BigFixed() {
		return detail::BigFixedLiteral<Chars...>::value;
	}

} // namespace lbiz

#endif // LBIZ_USERDEFINEDLITERALS_HPP
