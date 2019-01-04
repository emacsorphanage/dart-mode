if (isRaining()) {
  you.bringRainCoat();
} else if (isSnowing()) {
  you.wearJacket();
} else {
  car.putTopDown();
}
var message = StringBuffer('Dart is fun');
for (var i = 0; i < 5; i++) {
  message.write('!');
}
var collection = [0, 1, 2];
for (var x in collection) {
  print(x); // 0 1 2
}
while (!isDone()) {
  doSomething();
}
do {
  printLine();
} while (!atEndOfPage());
while (true) {
  if (shutDownRequested()) break;
  processIncomingRequests();
}
for (int i = 0; i < candidates.length; i++) {
  var candidate = candidates[i];
  if (candidate.yearsExperience < 5) {
    continue;
  }
  candidate.interview();
}
candidates
    .where((c) => c.yearsExperience >= 5)
    .forEach((c) => c.interview());
var command = 'OPEN';
switch (command) {
  case 'CLOSED':
    executeClosed();
    break;
  case 'PENDING':
    executePending();
    break;
  case 'APPROVED':
    executeApproved();
    break;
  case 'DENIED':
    executeDenied();
    break;
  case 'OPEN':
    executeOpen();
    break;
  default:
    executeUnknown();
}
// Make sure the variable has a non-null value.
assert(text != null);

// Make sure the value is less than 100.
assert(number < 100);

// Make sure this is an https URL.
assert(urlString.startsWith('https'));
assert(urlString.startsWith('https'),
    'URL ($urlString) should start with "https".');
