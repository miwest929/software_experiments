from collections import deque

def is_palindrome(num):
  digit_stack = []
  digit_queue = deque()

  num = abs(num)
  while num > 0:
    digit = num % 10
    digit_stack.append(digit)
    digit_queue.append(digit)
    num = num / 10

  while len(digit_stack) > 0:
    next_stack_digit = digit_stack.pop()
    next_queue_digit = digit_queue.popleft()

    if (next_stack_digit != next_queue_digit):
      return False

  return True

print is_palindrome(1221)
print is_palindrome(123454321)
print is_palindrome(1229)
print is_palindrome(-2147483648)
