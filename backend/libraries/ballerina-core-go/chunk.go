package ballerina

type Chunk[T any] struct {
	Values  []T
	From    int
	To      int
	HasMore bool
}

func NewChunk[T any](values []T, from, to int, hasMore bool) Chunk[T] {
	return Chunk[T]{
		Values:  values,
		From:    from,
		To:      to,
		HasMore: hasMore,
	}
}

func MapChunk[T, U any](ts Chunk[T], f func(T) U) Chunk[U] {
	us := make([]U, len(ts.Values))
	for i := range ts.Values {
		us[i] = f(ts.Values[i])
	}
	return Chunk[U]{Values: us, HasMore: ts.HasMore}
}
func DefaultChunk[T any]() Chunk[T] {
	return Chunk[T]{Values: make([]T, 0), HasMore: false}
}
