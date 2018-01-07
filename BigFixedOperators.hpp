#ifndef LBIZ_BIGFIXEDOPERATOR_HPP
#define LBIZ_BIGFIXEDOPERATOR_HPP

#include "BigIntOperators.hpp"
#include "UserDefinedLiterals.hpp"

namespace lbiz
{
	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> operator+( const BigFixed<N, Q>& n, const BigFixed<N, Q>& m ) {
		return BigFixed<N, Q> { n.data + m.data, ShiftTag<Q>{} };
	}

	template<std::size_t N, std::size_t Q, std::size_t M, std::size_t MQ>
	constexpr BigFixed<max( N, M ), max( Q, MQ )> operator+( const BigFixed<N, Q>& n, const BigFixed<M, MQ>& m ) {
		return static_cast<BigFixed<max( N, M ), max( Q, MQ )>>( n ) + static_cast<BigFixed<max( N, M ), max( Q, MQ )>>( m );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> operator-( const BigFixed<N, Q>& n, const BigFixed<N, Q>& m ) {
		return BigFixed<N, Q>( n.data - m.data, ShiftTag<Q>{} );
	}

	template<std::size_t N, std::size_t Q, std::size_t M, std::size_t MQ>
	constexpr BigFixed<max( N, M ), max( Q, MQ )> operator-( const BigFixed<N, Q>& n, const BigFixed<M, MQ>& m ) {
		return static_cast<BigFixed<max( N, M ), max( Q, MQ )>>( n ) - static_cast<BigFixed<max( N, M ), max( Q, MQ )>>( m );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> operator-( const BigFixed<N, Q>& n ) {
		return BigFixed<N, Q>( -n.data, ShiftTag<Q>{} );
	}

	template<std::size_t N, std::size_t Q, std::size_t M, std::size_t MQ, lfl::index_t... Index>
	constexpr BigInt<N + Q> round( const BigInt<M + N + Q + MQ>& n, lfl::index_tuple<Index...> ) {
		return n[Q - 1] * 2 < Base ? BigInt<N + Q>{ MaxValueTag { n.max_value }, MinValueTag { n.min_value }, n[Index + MQ]... }
		                           : BigInt<N + Q>{ MaxValueTag { n.max_value + 1 }, MinValueTag { n.min_value }, n[Index + MQ]... } +BigInt<1>( lbiz::MaxValueTag{1}, lbiz::MinValueTag{0}, 1 );
								   // TO DO -0.5ULP
	}

	template<std::size_t M, std::size_t N, lfl::index_t... Index>
	constexpr BigInt<M> roundzero( const BigInt<N>& n, lfl::index_tuple<Index...> ) {
		return BigInt<M> { MaxValueTag { n.max_value }, MinValueTag { n.min_value }, n[N - M + Index]... };
	}

	template<std::size_t MQ, std::size_t N, std::size_t Q>
	constexpr BigFixed<N, MQ> roundzero( const BigFixed<N, Q>& n ) {
		return BigFixed<N, MQ> { roundzero<N+MQ>( n.data, lfl::make_index_tuple<N + MQ>{} ), ShiftTag<MQ>{} };
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> operator*( const BigFixed<N, Q>& n, const BigFixed<N, Q>& m ) {
		return BigFixed<N, Q>( round<N, Q, N, Q>( sign_normalize( static_cast<BigInt<( N + Q ) * 2>>( n.data ) * static_cast<BigInt<( N + Q ) * 2>>( m.data ) ), lfl::make_index_tuple<N + Q>{} ), ShiftTag<Q>{} );
	}

	template<std::size_t N, std::size_t Q, std::size_t M, std::size_t MQ>
	constexpr BigFixed<N, Q> operator*( const BigFixed<N, Q>& n, const BigFixed<M, MQ>& m ) {
		return BigFixed<N, Q>( round<N, Q, M, MQ>( sign_normalize( static_cast<BigInt<N + Q + M + MQ>>( n.data ) * m.data ), lfl::make_index_tuple<N + Q>{} ), ShiftTag<Q>{} );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> square( const BigFixed<N, Q>& n ) {
		return BigFixed<N, Q>( round<N, Q, N, Q>( sign_normalize( square( static_cast<BigInt<( N + Q ) * 2>>( n.data ) ) ), lfl::make_index_tuple<N + Q>{} ), ShiftTag<Q>{} );
	}




	template<std::size_t N, std::size_t Q, std::ptrdiff_t... Index>
	constexpr BigFixed<N, Q> approx_inverse( const IndexedBigFixed<N, Q, Index...>& a, std::size_t leading_zeros ) {
		return BigFixed<N, Q>( BigInt<N + Q>{ MaxValueTag { Base-1 }, MinValueTag { -Base+1 }, ( Index == (std::ptrdiff_t)leading_zeros -  (std::ptrdiff_t)N ? Base / ( a[( N - 1 ) - leading_zeros] + 1) : 0 )...}, ShiftTag<Q>{} );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> inverse_loop( const BigFixed<N, Q>& a, const BigFixed<N, Q>& x, std::size_t i ) {
		return i == 0 ? x + x - a * square(x)
		              : inverse_loop( a, x + x - a * square(x), i / 2 );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> inverse( const BigFixed<N, Q>& a ) {
		return inverse_loop( a, approx_inverse( a, leading_zeros( a.data ) ), N + Q );
	}

	template<std::size_t N, std::size_t Q, std::ptrdiff_t... Index>
	constexpr BigFixed<N, Q> seed_invsqrt( const IndexedBigFixed<N, Q, Index...>& a, std::size_t leading_zeros ) {
		return BigFixed<N, Q>( BigInt<N + Q>{ MaxValueTag { 1 }, MinValueTag { 0 }, ( Index == ( ( std::ptrdiff_t )leading_zeros - (std::ptrdiff_t)N - 1 ) / 2 ? 1 : 0 )... }, ShiftTag<Q>{} );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> invsqrt_loop_expand( const BigFixed<N, Q>& a, const BigFixed<N, Q>& x ) {
		return x * ( 3.0_to_BigFixed - a * square(x) ) * 0.5_to_BigFixed;
	}

	template<std::size_t N, std::size_t Q, std::size_t MQ>
	constexpr BigFixed<N, Q> invsqrt_loop_expand( const BigFixed<N, Q>& a, const BigFixed<N, MQ>& x ) {
		return invsqrt_loop_expand( a, BigFixed<N, MQ*2+N>( x * ( 3.0_to_BigFixed - roundzero<MQ>( a ) * square(x) ) * 0.5_to_BigFixed ) );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> invsqrt_loop_conv( const BigFixed<N, Q>& a, const BigFixed<N, Q>& x, std::size_t i ) {
		return i == 0 ? x
		              : invsqrt_loop_conv( a, x * ( 3.0_to_BigFixed - a * square(x) ) * 0.5_to_BigFixed, i / 2 );
	}


	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> approx_invsqrt( const BigFixed<N, Q>& a ) {
		return invsqrt_loop_conv( a, seed_invsqrt( a, leading_zeros( a.data ) ), Base * Base * 4 );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> invsqrt( const BigFixed<N, Q>& a ) {
		return invsqrt_loop_expand( a, invsqrt_loop_expand( a, approx_invsqrt( roundzero<8-N>(a) ) ) );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> sqrt( const BigFixed<N, Q>& a ) {
		return a * invsqrt( a );
	}

	template<std::size_t N, std::size_t Q>
	constexpr BigFixed<N, Q> sign_normalize( const BigFixed<N, Q>& a ) {
		return BigFixed<N, Q>( sign_normalize( a.data ), ShiftTag<Q>{} );
	}
} // namespace lbiz

#endif // LBIZ_BIGFIXEDOPERATOR_HPP
