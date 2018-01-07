#ifndef LBIZ_BIGFIXED_HPP
#define LBIZ_BIGFIXED_HPP

#include "lfl/lfl.hpp"
#include "BigInt.hpp"
#include "BigIntOperators.hpp"

namespace lbiz
{
	template<std::size_t S>
	struct ShiftTag { };

	template<std::size_t N, std::size_t Q, std::ptrdiff_t... Index>
	struct IndexedBigFixed
	{
		using type = IndexedBigFixed;

		BigInt<N+Q> data;

		constexpr underlying_type operator[]( std::ptrdiff_t n ) const { return n < -(std::ptrdiff_t)Q || n >= (std::ptrdiff_t)N ? 0 : data[n + (std::ptrdiff_t)Q]; }


		template<std::size_t M, lfl::index_t... IndexM, std::size_t S, typename std::enable_if<M<= N+S && S <= Q, std::nullptr_t>::type = nullptr>
		constexpr explicit IndexedBigFixed( const IndexedBigInt<M, IndexM...>& m, ShiftTag<S> ) : data { MaxValueTag{ m.max_value }, MinValueTag{ m.min_value }, m[Index + ( std::ptrdiff_t )S]... } { }

		template<std::size_t M, std::size_t MQ, std::ptrdiff_t... IndexM, typename std::enable_if<M <= N && MQ <= Q, std::nullptr_t>::type = nullptr>
		constexpr explicit IndexedBigFixed( const IndexedBigFixed<M, MQ, IndexM...>& m ) : data { MaxValueTag { m.data.max_value }, MinValueTag { m.data.min_value },  m[Index]... } { }
	};

	namespace detail
	{
		template<std::size_t N, std::size_t Q, class Seq>
		struct ConstructIndexedBigFixed;
		template<std::size_t N, std::size_t Q, lfl::index_t... Index>
		struct ConstructIndexedBigFixed<N, Q, lfl::index_tuple<Index...>> : public IndexedBigFixed<N, Q, ((std::ptrdiff_t)Index- ( std::ptrdiff_t )Q)...> { };

		template<std::size_t N, std::size_t Q>
		using ConstructIndexedBigFixed_t = typename ConstructIndexedBigFixed<N, Q, lfl::make_index_tuple<N+Q>>::type;
	}

	template<std::size_t N, std::size_t Q>
	struct BigFixed : public lbiz::detail::ConstructIndexedBigFixed_t<N, Q>
	{
		template<std::size_t M, std::size_t S, typename std::enable_if<M <= N + S && S <= Q, std::nullptr_t>::type = nullptr>
		constexpr explicit BigFixed( const BigInt<M>& m, ShiftTag<S> s ) : lbiz::detail::ConstructIndexedBigFixed_t<N, Q> { m, s } { }
		template<std::size_t M, std::size_t MQ, typename std::enable_if<M <= N && MQ <= Q, std::nullptr_t>::type = nullptr>
		constexpr explicit BigFixed( const BigFixed<M, MQ>& m ) : lbiz::detail::ConstructIndexedBigFixed_t<N, Q> { m } { }
	};

} // namespace lbiz

#endif // LBIZ_BIGFIXED_HPP
