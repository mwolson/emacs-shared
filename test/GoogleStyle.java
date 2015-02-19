// This file acts like a unit test of sorts for google-c-style.el:
// If any file contents are changed after C-M-\, then the tests fail.
// Each test case is preceded with a comment.
//
// Use 'C-c C-s' on a line to understand which cc-mode syntactic rule(s) are in effect.
package com.company;

import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

public class CachingCrudClient
    // Test: implements keyword is +4
    implements CrudClient
    // Test: extends keyword is +4
    extends CrudClienter {

  private final LoadingCache<Long, Venue>
      // Test: wrapping after a type and before a variable name is +4
      venueCache =
      // Test: further line continuation wraps remain on this column
      CacheBuilder // Test: the next comment will be +4
          // Test: the next comment will be aligned with this one
          // Test: further line continuation wraps remain on this column
          .newBuilder()
          .expireAfterWrite(1, TimeUnit.DAYS)
          .lineContinuer(
              // Test: inside of class expressions will be +2
              new Foo(),
              new Bar())
          .build(new CacheLoader<Long, Venue>() {
            // Test: inside of class expressions will be +2
            public Venue load(Long venueId) {
              return delegate.getVenue(venueId);
            }
          });

  private final LoadingCache<Long, Venue> venueCache2 =
      CacheBuilder
          .newBuilder()
          .expireAfterWrite(1, TimeUnit.DAYS)
          .lineContinuer(new Foo(),
              // Test: this comment should be +4, not aligned with '('
              new Bar(),
              // Test: this comment should be aligned with previous line, not aligned with '('
              new Bar());

  private final LoadingCache<Long, Venue> venueCache3 =
      CacheBuilder
          .newBuilder()
          .expireAfterWrite(1, TimeUnit.DAYS)
          .lineContinuer(new Foo(),
              // Test: this comment should be +4, not aligned with '('
              new Bar(),
              // Test: this comment should be aligned with previous line, not aligned with '('
              new Bar());

  private int arithExpr1 = (4
                            // Test: this comment should be aligned 1 char after the '('
                            / 2); // Test: this should be aligned 1 char after the '('

  private int arithExpr2 = (4 /
                            2 // Test: this should be aligned 1 char after the '('
                            // Test: this comment should be aligned 1 char after the '('
                            / 2); // Test: this should be aligned 1 char after the '('

  private String stringExpr1 = ("foo" +
                                // a comment
                                "bar"
                                + "bar"); // Test: this should be aligned 1 char after the '('

  int product = 1
                // Test: this comment should be aligned 2 char after the '='
                * 2 * 2 // Test: this should be aligned 2 char after the '='
                * 3
                * 4;

  public CachingCrudClient() {}

  public boolean boolMethod() {
    return 1 == 1
           && (0 != 1) // Test: this should be aligned to end of return statement plus whitespace
           // Test: this comment should align siimilarly
           && 2 != 1;
  }
}
