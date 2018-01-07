#include "lfl/lfl.hpp"
#include "BigFixed.hpp"
#include "BigFixedOperators.hpp"
#include "UserDefinedLiterals.hpp"
#include "ToString.hpp"

#include <iostream>


template<std::size_t MutualRecursion, std::size_t N, std::size_t Q>
constexpr lbiz::BigFixed<N, Q> Gauss_Legendre_loop_sub( const lbiz::BigFixed<N, Q>& a, const lbiz::BigFixed<N, Q>& b, const lbiz::BigFixed<N, Q>& t, const lbiz::BigFixed<N, Q>& p, const lbiz::BigFixed<N, Q>& new_a );

template<std::size_t MutualRecursion, std::size_t N, std::size_t Q, typename std::enable_if<MutualRecursion == 0, std::nullptr_t>::type = nullptr>
constexpr lbiz::BigFixed<N, Q> Gauss_Legendre_loop( const lbiz::BigFixed<N, Q>& a, const lbiz::BigFixed<N, Q>& b, const lbiz::BigFixed<N, Q>& t, const lbiz::BigFixed<N, Q>& p ) {
	using lbiz::operator"" _to_BigFixed;
	return lbiz::square( a + b ) * lbiz::inverse( t ) * 0.25_to_BigFixed;
}

template<std::size_t MutualRecursion, std::size_t N, std::size_t Q, typename std::enable_if<MutualRecursion != 0, std::nullptr_t>::type = nullptr>
constexpr lbiz::BigFixed<N, Q> Gauss_Legendre_loop( const lbiz::BigFixed<N, Q>& a, const lbiz::BigFixed<N, Q>& b, const lbiz::BigFixed<N, Q>& t, const lbiz::BigFixed<N, Q>& p ) {
	using lbiz::operator"" _to_BigFixed;
	return Gauss_Legendre_loop_sub<MutualRecursion - 1>( a, b, t, p, ( a + b ) * 0.5_to_BigFixed );
}

template<std::size_t MutualRecursion, std::size_t N, std::size_t Q>
constexpr lbiz::BigFixed<N, Q> Gauss_Legendre_loop_sub( const lbiz::BigFixed<N, Q>& a, const lbiz::BigFixed<N, Q>& b, const lbiz::BigFixed<N, Q>& t, const lbiz::BigFixed<N, Q>& p, const lbiz::BigFixed<N, Q>& new_a ) {
	using lbiz::operator"" _to_BigFixed;
	return Gauss_Legendre_loop<MutualRecursion>(
		new_a,
		lbiz::sqrt( a * b ),
		t - p * lbiz::square( a - new_a ),
		p * 2.0_to_BigFixed
	);
}


int main() {
	static const constexpr std::size_t l = 6;
	static const constexpr std::size_t bits = 1 << l;
	using fp_type = lbiz::BigFixed<2, bits-2>;
	using lbiz::operator"" _to_BigFixed;

	constexpr fp_type two = fp_type( 2.0_to_BigFixed );
	constexpr fp_type a = fp_type( 1.0_to_BigFixed );
	constexpr auto b = lbiz::invsqrt( two );
	constexpr fp_type t = fp_type( 0.25_to_BigFixed );
	constexpr fp_type p = fp_type( 1.0_to_BigFixed );
	constexpr auto c = Gauss_Legendre_loop<l>( a, b, t, p );
	constexpr auto pi = sign_normalize( c );
	constexpr auto pi_string = lbiz::to_string( pi );
	
	struct count_nonzero { constexpr std::ptrdiff_t operator()( std::ptrdiff_t n, char c ) const { return n > 0 ? n : c == '0' ? n - 1 : -n; } };
	constexpr std::size_t start = lfl::reduce( 0, pi_string, count_nonzero{} );

	std::cout << pi_string.data() + start << std::endl;
	return 0;
}
