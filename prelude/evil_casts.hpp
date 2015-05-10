#ifndef EVIL_CASTS_HPP
#define EVIL_CASTS_HPP

// Consider   some_evil_cast<char const *>("foo")   . There are two possible interpretations for the result: either the array should decay into a pointer and be immediately returned, or the bytes making up the array are interpreted as the bytes making up the to-be-returned pointer. cast_dammit_cast does the former, while savage_cast does the latter.

template <typename To, typename From> To savage_cast (From const & from)
{ return (To const &) from; }
  // Can even be used to check endianness:  geordi << hex << savage_cast<uint32_t>("\xef\xbe\xad\xde"))

template <typename To, typename From> To cast_dammit_cast (From const from)
{
  From const & r = from; // from itself is not a reference because we want arrays to decay, so that cast_dammit_cast<char*>("oi") works properly.
  return (To const &) r;
}

#endif // header guard
