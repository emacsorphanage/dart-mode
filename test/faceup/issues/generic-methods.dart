abstract class Stream<T> {

  static Stream<T> castFrom<S, T>(Stream<S> source) =>
      new CastStream<S, T>(source);

  Stream<S> map<S>(S convert(T event)) {
    return new _MapStream<T, S>(this, convert);
  }

  Stream<E> asyncMap<E>(FutureOr<E> convert(T event)) {
    _StreamControllerBase<E> controller;
    StreamSubscription<T> subscription;

    void onListen() {
      final add = controller.add;
      assert(controller is _StreamController<E> ||
          controller is _BroadcastStreamController);
      final addError = controller._addError;
      subscription = this.listen((T event) {
        FutureOr<E> newValue;
        try {
          newValue = convert(event);
        } catch (e, s) {
          controller.addError(e, s);
          return;
        }
        if (newValue is Future<E>) {
          subscription.pause();
          newValue
              .then(add, onError: addError)
              .whenComplete(subscription.resume);
        } else {
          controller.add(newValue);
        }
      }, onError: addError, onDone: controller.close);
    }

    if (this.isBroadcast) {
      controller = new StreamController<E>.broadcast(
          onListen: onListen,
          onCancel: () {
            subscription.cancel();
          },
          sync: true);
    } else {
      controller = new StreamController<E>(
          onListen: onListen,
          onPause: () {
            subscription.pause();
          },
          onResume: () {
            subscription.resume();
          },
          onCancel: () => subscription.cancel(),
          sync: true);
    }
    return controller.stream;
  }

  Stream<E> asyncExpand<E>(Stream<E> convert(T event)) {
    _StreamControllerBase<E> controller;
    StreamSubscription<T> subscription;
    void onListen() {
      assert(controller is _StreamController ||
          controller is _BroadcastStreamController);
      subscription = this.listen((T event) {
        Stream<E> newStream;
        try {
          newStream = convert(event);
        } catch (e, s) {
          controller.addError(e, s);
          return;
        }
        if (newStream != null) {
          subscription.pause();
          controller.addStream(newStream).whenComplete(subscription.resume);
        }
      },
          onError: controller._addError, // Avoid Zone error replacement.
          onDone: controller.close);
    }

    if (this.isBroadcast) {
      controller = new StreamController<E>.broadcast(
          onListen: onListen,
          onCancel: () {
            subscription.cancel();
          },
          sync: true);
    } else {
      controller = new StreamController<E>(
          onListen: onListen,
          onPause: () {
            subscription.pause();
          },
          onResume: () {
            subscription.resume();
          },
          onCancel: () => subscription.cancel(),
          sync: true);
    }
    return controller.stream;
  }

  Stream<S> expand<S>(Iterable<S> convert(T element)) {
    return new _ExpandStream<T, S>(this, convert);
  }

  Stream<S> transform<S>(StreamTransformer<T, S> streamTransformer) {
    return streamTransformer.bind(this);
  }

  Future<S> fold<S>(S initialValue, S combine(S previous, T element)) {
    _Future<S> result = new _Future<S>();
    S value = initialValue;
    StreamSubscription subscription;
    subscription = this.listen(
        (T element) {
          _runUserCode(() => combine(value, element), (S newValue) {
            value = newValue;
          }, _cancelAndErrorClosure(subscription, result));
        },
        onError: result._completeError,
        onDone: () {
          result._complete(value);
        },
        cancelOnError: true);
    return result;
  }

  Stream<R> cast<R>() => Stream.castFrom<T, R>(this);


  Future<E> drain<E>([E futureValue]) =>
      listen(null, cancelOnError: true).asFuture<E>(futureValue);

}


abstract class StreamTransformer<S, T> {

  static StreamTransformer<TS, TT> castFrom<SS, ST, TS, TT>(
      StreamTransformer<SS, ST> source) {
    return new CastStreamTransformer<SS, ST, TS, TT>(source);
  }

  StreamTransformer<RS, RT> cast<RS, RT>();
}

abstract class StreamTransformerBase<S, T> implements StreamTransformer<S, T> {
  const StreamTransformerBase();

  StreamTransformer<RS, RT> cast<RS, RT>() =>
      StreamTransformer.castFrom<S, T, RS, RT>(this);
}
