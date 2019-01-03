int fib(int n) => (n > 2) ? (fib(n - 1) + fib(n - 2)) : 1;
// this is a fibonacci function implementation with a ternary operator in Dart
// this code shall be read as:
// If int n > 2, return fib(n - 1) + fib(n - 2); 
// otherwise, return int 1 as result

void main() {
  print('fib(20) = ${fib(20)}');
}
