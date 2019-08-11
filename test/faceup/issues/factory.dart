abstract class Stream<T> {

  const factory Stream.empty() = _EmptyStream<T>;

}


abstract class StreamTransformer<S, T> {

  factory StreamTransformer.fromHandlers(
      {void handleData(S data, EventSink<T> sink),
      void handleError(Object error, StackTrace stackTrace, EventSink<T> sink),
      void handleDone(EventSink<T> sink)}) = _StreamHandlerTransformer<S, T>;

  factory StreamTransformer.fromBind(Stream<T> Function(Stream<S>) bind) =
      _StreamBindTransformer<S, T>;

}
