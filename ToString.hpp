#ifndef LBIZ_TOSTRING_HPP
#define LBIZ_TOSTRING_HPP

#include "BigInt.hpp"
#include "BigFixed.hpp"
#include "lfl/lfl.hpp"

namespace lbiz
{
	namespace detail
	{
		constexpr char to_digit( underlying_type n, std::size_t d ) {
			return d == 0 ? '0' + n % 10 : to_digit( n / 10, d - 1 );
		}

		template<std::size_t N, std::size_t Q, lfl::index_t... Index>
		constexpr lfl::array<char, N * BaseNumberOfDigits> to_string_integer_part_impl( BigFixed<N, Q> n, lfl::index_tuple<Index...> ) {
			return lfl::array<char, N * BaseNumberOfDigits> { { 
					to_digit( n[( N - 1 ) - Index / BaseNumberOfDigits], ( BaseNumberOfDigits - 1 ) - Index % BaseNumberOfDigits )...
			} };
		}

		template<std::size_t N, std::size_t Q, lfl::index_t... Index>
		constexpr lfl::array<char, Q * BaseNumberOfDigits + 2> to_string_fractional_part_impl( BigFixed<N, Q> n, lfl::index_tuple<Index...> ) {
			return lfl::array<char, Q * BaseNumberOfDigits + 2> { {
				'.',
				to_digit( n[-1-(std::ptrdiff_t)(Index / BaseNumberOfDigits)], ( BaseNumberOfDigits - 1 ) - Index % BaseNumberOfDigits )...,
				'\0'
			} };
		}

		template<std::size_t N, std::size_t Q>
		constexpr lfl::array<char, N * BaseNumberOfDigits> to_string_integer_part( BigFixed<N, Q> n ) {
			return to_string_integer_part_impl( n, lfl::make_index_tuple<N * BaseNumberOfDigits>{} );
		}

		template<std::size_t N, std::size_t Q>
		constexpr lfl::array<char, Q * BaseNumberOfDigits + 2> to_string_fractional_part( BigFixed<N, Q> n ) {
			return to_string_fractional_part_impl( n, lfl::make_index_tuple<Q * BaseNumberOfDigits>{} );
		}
	}

	template<std::size_t N, std::size_t Q>
	constexpr lfl::array<char, ( N + Q ) * BaseNumberOfDigits + 2> to_string( BigFixed<N, Q> n ) {
		return lfl::concat( detail::to_string_integer_part( n ), detail::to_string_fractional_part( n ) );
	}

} // namespace lbiz

#endif // LBIZ_TOSTRING_HPP
