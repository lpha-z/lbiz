#ifndef LBIZ_BIGINT_HPP
#define LBIZ_BIGINT_HPP

#include <cstdint>
#include "lfl/lfl.hpp"

namespace lbiz
{
	using underlying_type = std::int64_t;

	namespace detail
	{
		constexpr std::size_t log10( underlying_type n ) {
			return n / 10 ? log10( n / 10 ) + 1 : 0;
		}
	} // namespace detail

	static constexpr underlying_type Base = 100;
	static constexpr std::size_t BaseNumberOfDigits = detail::log10( Base );


	struct MaxValueTag { underlying_type val; };
	struct MinValueTag { underlying_type val; };

	template<std::size_t N, lfl::index_t... Index>
	struct IndexedBigInt
	{
		using type = IndexedBigInt;

		lfl::array<underlying_type, N> data;
		underlying_type max_value;
		underlying_type min_value;

		constexpr underlying_type operator[]( int n ) const { return n < 0 || n >= N ? 0 : data[( std::size_t )n]; }

		template<typename... Args>
		constexpr explicit IndexedBigInt( MaxValueTag tmax, MinValueTag tmin, Args... args ) : data { { args... } }, max_value { tmax.val }, min_value { tmin.val } { }

		template<std::size_t M, lfl::index_t... IndexM, typename std::enable_if<M <= N, std::nullptr_t>::type = nullptr>
		constexpr explicit IndexedBigInt( const IndexedBigInt<M, IndexM...>& m ) : data { { m[IndexM]... } }, max_value { m.max_value }, min_value { m.min_value } { }
	};

	namespace detail
	{
		template<class Seq>
		struct ConstructIndexedBigInt;
		template<lfl::index_t... Index>
		struct ConstructIndexedBigInt<lfl::index_tuple<Index...>> : public IndexedBigInt<sizeof...( Index ), Index...> { };

		template<std::size_t N>
		using ConstructIndexedBigInt_t = typename ConstructIndexedBigInt<lfl::make_index_tuple<N>>::type;
	}

	template<std::size_t N>
	struct BigInt : public lbiz::detail::ConstructIndexedBigInt_t<N>
	{
		template<typename... Args>
		constexpr explicit BigInt( MaxValueTag tmax, MinValueTag tmin, Args... args ) : lbiz::detail::ConstructIndexedBigInt_t<N> { tmax, tmin, args... } { }

		template<std::size_t M, typename std::enable_if<M <= N, std::nullptr_t>::type = nullptr>
		constexpr explicit BigInt( const BigInt<M>& m ) : lbiz::detail::ConstructIndexedBigInt_t<N> { static_cast<const lbiz::detail::ConstructIndexedBigInt_t<M>&>(m) } { }

		constexpr explicit BigInt( underlying_type x ) : lbiz::detail::ConstructIndexedBigInt_t<N> { MaxValueTag{ x < 0 ? 0 : x }, MinValueTag{ x > 0 ? 0 : -x }, x } { }
	};

} // namespace lbiz

#endif // LBIZ_BIGINT_HPP
