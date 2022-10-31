void myFunction()
{
  int something     = 0,
      stuff         = 1,
      pizza_is_good = 0,
      i_love_pizza  = 1;

  if (something) { //<--- I want to jump to this brace!
    // do lots of stuff
    if (stuff) {
      // stuff
    }
    // more stuff
    // ...

    // I want to put my cursor somewhere on this line <---
    // (anywhere just outside the following if) and call c-beginning-of-block
    // and jump to the brace marked above (skipping "sibling" statements)
    if (pizza_is_good) {
      // do something
      // wait, where am I?
    }
    // way more stuff
    // ...
    if (i_love_pizza) {
      // eat pizza
    }
  }
}
