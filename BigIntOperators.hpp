#ifndef LBIZ_BIGINTOPERATORS_HPP
#define LBIZ_BIGINTOPERATORS_HPP

#include "BigInt.hpp"

namespace lbiz
{
	template<class T>
	constexpr T max( const T& a, const T& b ) { return a < b ? b : a; }

	constexpr std::size_t dot_sub( std::size_t x, std::size_t y ) {
		return x < y ? 0 : x - y;
	}

	template<class Local, class Pred, class Func, class... Args>
	constexpr Local hold_local( const Local& local, const Pred& pred, Func func, Args&&... args ) {
		// std::forward is not constexpr function in C++11
		return pred( local ) ? local : (*func)( static_cast<Args&&>( args )... );
	}

	// --- Carry Type ---
	enum class CarryType
	{
		BorrowGenerate,
		BorrowTemporal,
		BorrowPass,
		None,
		CarryPass,
		CarryTemporal,
		CarryGenerate,
	};
	constexpr CarryType propagate( CarryType c, CarryType ref ) {
		return c == CarryType::BorrowTemporal ? ref == CarryType::CarryGenerate  ? CarryType::None :
		                                        ref == CarryType::CarryTemporal  ? CarryType::BorrowTemporal :
		                                        ref == CarryType::CarryPass      ? CarryType::BorrowTemporal
		                                                                         : CarryType::BorrowGenerate :
		       c == CarryType::BorrowPass     ? ref == CarryType::CarryGenerate  ? CarryType::None :
		                                        ref == CarryType::CarryTemporal  ? CarryType::None :
		                                        ref == CarryType::CarryPass      ? CarryType::None
		                                                                         : ref :
		       c == CarryType::CarryPass      ? ref == CarryType::BorrowGenerate ? CarryType::None :
		                                        ref == CarryType::BorrowTemporal ? CarryType::None :
		                                        ref == CarryType::BorrowPass     ? CarryType::None
		                                                                         : ref :
		       c == CarryType::CarryTemporal  ? ref == CarryType::BorrowGenerate ? CarryType::None :
		                                        ref == CarryType::BorrowTemporal ? CarryType::CarryTemporal :
		                                        ref == CarryType::BorrowPass     ? CarryType::CarryTemporal
		                                                                         : CarryType::CarryGenerate
		                                      : c;
	}

	// --- CarryArray ---
	template<std::size_t N>
	class CarryArray
	{
		CarryType c[N];

		template<lfl::index_t... Index>
		constexpr CarryArray( const CarryArray<N>& c, std::size_t step, lfl::index_tuple<Index...> ) :
			c { propagate( c.c[Index], Index < step ? CarryType::None : c.c[Index / step * step - 1] )... } { }

	public:
		constexpr underlying_type carry( std::size_t n ) const {
			return n == 0 ? 0
			              : c[n-1] == CarryType::BorrowGenerate ? -1 :
			                c[n-1] == CarryType::CarryGenerate  ?  1 : 0;
		}

		template<lfl::index_t... Index>
		explicit constexpr CarryArray( const IndexedBigInt<N, Index...>& n ) :
			c { ( Index == 0 ? n[Index] <= -Base   ? CarryType::BorrowGenerate :
			                   n[Index] >=  Base   ? CarryType::CarryGenerate
			                                       : CarryType::None
			                 : n[Index] <  -Base   ? CarryType::BorrowGenerate :
			                   n[Index] == -Base   ? CarryType::BorrowTemporal :
			                   n[Index] == -Base+1 ? CarryType::BorrowPass :
			                   n[Index] ==  Base-1 ? CarryType::CarryPass :
			                   n[Index] ==  Base   ? CarryType::CarryTemporal :
			                   n[Index] >   Base   ? CarryType::CarryGenerate
			                                       : CarryType::None
			    )... } { }

		template<lfl::index_t... Index>
		constexpr CarryArray( const IndexedBigInt<N, Index...>& n, bool positive ) :
			c { ( positive ? n[Index] <  0 ? CarryType::BorrowGenerate :
			                 n[Index] == 0 ? CarryType::BorrowPass
			                               : CarryType::None
			               : n[Index] >  0 ? CarryType::CarryGenerate :
			                 n[Index] == 0 ? CarryType::CarryPass
			                               : CarryType::None
			    )... } { }

		static constexpr CarryArray<N> propagate_loop( const CarryArray<N>& c, std::size_t step ) {
			return step >= N ? c
			                 : propagate_loop( CarryArray<N>( c, step, lfl::make_index_tuple<N>{} ), step * 2 );
		}

		constexpr CarryArray<N> propagated() const {
			return propagate_loop( *this, 1 );
		}

	};

	// --- normalize ---

	template<std::size_t N, lfl::index_t... Index>
	constexpr BigInt<N> propagate_carry( const IndexedBigInt<N, Index...>& n ) {
		return BigInt<N> { MaxValueTag { ( Base - 1 ) + n.max_value / Base }, MinValueTag { ( -Base + 1 ) - n.min_value / Base }, ( n[Index] % Base + n[( std::ptrdiff_t )Index - 1] / Base )... };
	}

	template<std::size_t N>
	constexpr BigInt<N> cs_normalize( const BigInt<N>& n ) {
		return n.max_value <= Base && n.min_value >= -Base ? n
		                                                   : cs_normalize( propagate_carry( n ) );
	}



	template<std::size_t N, lfl::index_t... Index>
	constexpr BigInt<N> normalize_add_carry( const IndexedBigInt<N, Index...>& n, const CarryArray<N>& c ) {
		// assert( n.max_value <= Base && m.min_value >= -Base );
		return BigInt<N> { MaxValueTag { Base - 1 }, MinValueTag { -Base + 1 }, ( ( n[Index] + c.carry( Index ) ) % Base )... };
	}

	template<std::size_t N>
	constexpr BigInt<N> normalize( const BigInt<N>& n ) {
		return n.max_value <= Base && n.min_value >= -Base ? normalize_add_carry( n, CarryArray<N>( n ).propagated() )
		                                                   : normalize( propagate_carry( n ) );
	}

	template<std::size_t N, lfl::index_t... Index>
	constexpr BigInt<N> normalize_add_borrow( const IndexedBigInt<N, Index...>& n, const CarryArray<N>& c, bool positive ) {
		// assert( n.max_value < Base && n.min_value > -Base );
		return positive ? BigInt<N> { MaxValueTag { Base - 1 }, MinValueTag { 0 }, ( ( n[Index] + c.carry( Index ) + Base ) % Base )... }
		                : BigInt<N> { MaxValueTag { 0 }, MinValueTag { -Base + 1 }, ( ( n[Index] + c.carry( Index ) - Base ) % Base )... };
	}

	struct sign_bp_pred
	{
		constexpr bool operator()( std::size_t y ) const { return y != 0; };
	};

	template<std::size_t N>
	constexpr underlying_type sign_bp_loop( const BigInt<N>& n, std::size_t begin, std::size_t end ) {
		return begin + 1 == end ? n[N - 1 - begin]
		                        : hold_local( sign_bp_loop( n, begin, ( begin + end ) / 2 ), sign_bp_pred { }, &sign_bp_loop<N>, n, ( begin + end ) / 2, end );
	}

	template<std::size_t N>
	constexpr underlying_type sign( const BigInt<N>& n ) {
		return sign_bp_loop( n, 0, N );
	}

	template<std::size_t N>
	constexpr BigInt<N> sign_normalize( const BigInt<N>& n ) {
		return n.max_value == Base-1 && n.min_value == -Base+1 ? normalize_add_borrow( n, CarryArray<N>( n, sign( n ) > 0 ).propagated(), sign( n ) > 0 )
		                                                       : sign_normalize( normalize( n ) );
	}



	template<std::size_t N, std::size_t M, lfl::index_t... Index>
	constexpr BigInt<max( N, M )> add( const BigInt<N>& n, const BigInt<M>& m, lfl::index_tuple<Index...> ) {
		return BigInt<max( N, M )> { MaxValueTag { n.max_value + n.max_value }, MinValueTag { n.min_value + m.min_value }, ( n[Index] + m[Index] )... };
	}
	template<std::size_t N, std::size_t M>
	constexpr BigInt<max( N, M )> operator+( const BigInt<N>& n, const BigInt<M>& m ) {
		return /* sign_normalize( normalize( propagate_carry( */ add( n, m, lfl::make_index_tuple<max( N, M )>{} )/* ) ) ) */;
	}

	template<std::size_t N, lfl::index_t... Index>
	constexpr BigInt<N> operator-( const IndexedBigInt<N, Index...>& n ) {
		return BigInt<N> { MaxValueTag { -n.min_value }, MinValueTag { -n.max_value }, ( -n[Index] )... };
	}

	template<std::size_t N, std::size_t M>
	constexpr BigInt<max( N, M )> operator-( const BigInt<N>& n, const BigInt<M> m ) {
		return n + ( -m );
	}




	// fast modulo transform
	// w^N == 1 mod p

	template<class T, std::size_t N, T w, T p>
	class FMT
	{
		// mod_inv
		static constexpr T mod_inv( T a, T m ) {
			return a == 1 ? 1
			            : ( ( 1 - m * mod_inv( m%a, a ) ) / a + m ) % m;
		}

		static constexpr T inv_N = mod_inv( N, p );

		// make_coeff_array
		template<std::size_t M, lfl::index_t... Index>
		static constexpr lfl::array<T, M * 2> make_coeff_array_impl( const lfl::array<T, M>& arr, lfl::index_tuple<Index...> ) {
			return lfl::array<T, M * 2> { { arr[( std::size_t )Index]..., ( w * arr[( std::size_t )( M - 1 )] % p * arr[( std::size_t )Index] % p )... } }; // for C2666
		}

		template<std::size_t M, typename std::enable_if<M == 1, std::nullptr_t>::type = nullptr>
		static constexpr lfl::array<T, 1> make_coeff_array() {
			return lfl::array<T, 1> { { 1 } };
		}
		template<std::size_t M, typename std::enable_if<M != 1, std::nullptr_t>::type = nullptr>
		static constexpr lfl::array<T, M> make_coeff_array() {
			return make_coeff_array_impl( make_coeff_array<M / 2>(), lfl::make_index_tuple<M / 2>{} );
		}
		static constexpr lfl::array<T, N> coeff_array = make_coeff_array<N>();


		// fmt_loop
		template<bool inv>
		static constexpr T butterfly_result( const lfl::array<T, N>& arr, std::size_t step, std::size_t i, std::size_t j, bool sub ) {
			return arr[i*step + j] + ( sub ? coeff_array[N / 2] : 1 ) * coeff_array[inv ? ( N - ( j *( N / 2 / step ) ) ) % N : j *( N / 2 / step )] % p * arr[i*step + j + N / 2];
		}

		template<bool inv, lfl::index_t... Index>
		static constexpr lfl::array<T, N> butterfly( const lfl::array<T, N>& arr, std::size_t step, lfl::index_tuple<Index...> ) {
			return lfl::array<T, N> { { ( butterfly_result<inv>( arr, step, Index / ( step * 2 ), Index%step, ( Index&step ) != 0 ) % p )... } };
		}

		template<bool inv>
		static constexpr lfl::array<T, N> fmt_loop( const lfl::array<T, N>& arr, std::size_t step ) {
			return step == N ? arr
			                 : fmt_loop<inv>( butterfly<inv>( arr, step, lfl::make_index_tuple<N>{} ), step * 2 );
		}

		// ifmt_impl
		template<lfl::index_t... Index>
		static constexpr BigInt<N> ifmt_impl( const lfl::array<T, N>& arr, lfl::index_tuple<Index...> ) {
			return BigInt<N> { MaxValueTag { p / 2 }, MinValueTag { -p / 2 }, ( ( arr[( std::size_t )Index] * inv_N % p + 3 * p / 2 ) % p - p / 2 )... };
		}
	public:
		static constexpr lfl::array<T, N> fmt( const BigInt<N>& n ) {
			return fmt_loop<false>( n.data, 1 );
		}
		static constexpr BigInt<N> ifmt( const lfl::array<T, N>& arr ) {
			return ifmt_impl( fmt_loop<true>( arr, 1 ), lfl::make_index_tuple<N>{} );
		}

	};

	template<class T, std::size_t N, T w, T p>
	constexpr lfl::array<T, N> FMT<T, N, w, p>::coeff_array;// = FMT<T, N, w, p>::make_coeff_array<N>();


	template<std::size_t N>
	class FMT_instance
	{
		static constexpr underlying_type n = 1 << 17;
		static constexpr underlying_type p = 149 * n + 1;;
		static constexpr underlying_type w = 770;

		static constexpr underlying_type sq( underlying_type x ) { return x*x; }
		static constexpr underlying_type calc_w( underlying_type a ) {
			return a == n ? w
			              : sq( calc_w( a * 2 ) ) % p;
		}
		using fmt = FMT<underlying_type, N, calc_w( N ), p>;
		FMT<underlying_type, N, calc_w( N ), p> _obj;
		//FMT<underlying_type, 4, 10, 101> fmt;
		//FMT<underlying_type, 8, 7, 1201> fmt;

		template<class T>
		struct mult
		{
			constexpr T operator() ( const T& a, const T& b ) const { return a * b % FMT_instance::p; }
		};

		template<class T>
		struct square_fun
		{
			constexpr T operator() ( const T& a ) const { return a * a % FMT_instance::p; }
		};
	public:
		static constexpr BigInt<N> mul( const BigInt<N>& n, const BigInt<N>& m ) {
			return fmt::ifmt( lfl::zip_with( fmt::fmt( n ), fmt::fmt( m ), mult<underlying_type> { } ) );
		}

		static constexpr BigInt<N> square( const BigInt<N>& n ) {
			return fmt::ifmt( lfl::map( fmt::fmt( n ), square_fun<underlying_type>{ } ) );
		}
	};

	template<std::size_t N>
	constexpr BigInt<N> operator* ( const BigInt<N>& n, const BigInt<N>& m ) {
		return FMT_instance<N>::mul( cs_normalize( n ), cs_normalize( m ) );
	}

	template<std::size_t N>
	constexpr BigInt<N> square( const BigInt<N>& n ) {
		return FMT_instance<N>::square( cs_normalize( n ) );
	}

	template<std::size_t N, std::size_t M>
	constexpr underlying_type mul_reduce_loop( const BigInt<N>& n, const BigInt<M>& m, std::ptrdiff_t begin, std::ptrdiff_t end, std::ptrdiff_t rbegin, underlying_type acc ) {
		return begin + 1 == end ? acc + n[begin] * m[rbegin]
		                        : mul_reduce_loop( n, m, begin + 1, end, rbegin - 1, acc + n[begin] * m[rbegin] );
	}

	template<std::size_t N, std::size_t M, lfl::index_t... Index>
	constexpr BigInt<N> mul_impl( const BigInt<N>& n, const BigInt<M>& m, lfl::index_tuple<Index...> ) {
		return BigInt<N> { MaxValueTag { Base * M }, MinValueTag { Base * M }, mul_reduce_loop( n, m, (std::ptrdiff_t)Index - M + 1, Index + 1, M - 1, 0 )... };
	}

	template<std::size_t N, std::size_t M>
	constexpr BigInt<N> operator* ( const BigInt<N>& n, const BigInt<M>& m ) {
		return mul_impl( n, m, lfl::make_index_tuple<N>{} );
	}



	// --- leading_zeros ---

	struct leading_zeros_bp_pred
	{
		std::size_t x;
		constexpr leading_zeros_bp_pred( std::size_t x ) : x( x ) { }
		constexpr bool operator()( std::size_t y ) const { return y < x; };
	};

	template<std::size_t N>
	constexpr std::size_t leading_zeros_bp_loop( const BigInt<N>& n, std::size_t begin, std::size_t end ) {
		return begin + 1 == end ? n[N - 1 - begin] == 0 ? end
		                                                : begin
		                        : hold_local( leading_zeros_bp_loop( n, begin, ( begin + end ) / 2 ), leading_zeros_bp_pred { ( begin + end ) / 2 }, &leading_zeros_bp_loop<N>, n, ( begin + end ) / 2, end );
	}

	template<std::size_t N>
	constexpr std::size_t leading_zeros( const BigInt<N>& n ) {
		// assert( n.max_value < Base && n.min_value > -Base ); 
		return leading_zeros_bp_loop( n, 0, N );
	}

} // lbiz

#endif // LBIZ_BIGINTOPERATORS_HPP
