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
              new Foo(), // Test: inside of argument continuations will be +4
              new Bar())
          .build(new CacheLoader<Long, Venue>() {
            // Test: inside of class expressions will be +2, even when surrounded by function call
            public Venue load(Long venueId) {
              // Test: methods inside class expressions will be + 2
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

  private final LoadingCache<Long, Venue> venueCache3 = CacheBuilder
      // Test: this comment should be +4
      .newBuilder() // Test: this should be +4
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

  // Note: A comment on any part of the builder lines after first will cause IntelliJ to format to +4 instead,
  //       which is a bug that we don't emulate
  private static final Map<String, FeatureValueHydrator> HYDRATORS1 =
      // Test: builder calls are aligned on '.' even when generics are present
      ImmutableMap.<String, FeatureValueHydrator>builder()
                  .put("string", FeatureValueHydrator.String.INSTANCE)
                  .put("boolean", FeatureValueHydrator.Boolean.INSTANCE)
                  .build();

  private static final Map<String, FeatureValueHydrator> HYDRATORS2 =
      // Test: builder calls are aligned on the very first '.' even when generics are present
      ImmutableMap.swell().<String, FeatureValueHydrator>builder()
                  .put("string", FeatureValueHydrator.String.INSTANCE)
                  .put("boolean", FeatureValueHydrator.Boolean.INSTANCE)
                  .build();

  private static final Map<String, FeatureValueHydrator> HYDRATORS3 =
      // Test: builder calls are aligned on '.' even when empty generics are present
      ImmutableMap.<>builder()
                  .put("string", FeatureValueHydrator.String.INSTANCE)
                  .put("boolean", FeatureValueHydrator.Boolean.INSTANCE)
                  .build();

  private static final Map<String, FeatureValueHydrator> HYDRATORS4 =
      // Test: builder calls are aligned to the very first '.'
      ImmutableMap.swell().builder()
                  .put("string", FeatureValueHydrator.String.INSTANCE)
                  .put("boolean", FeatureValueHydrator.Boolean.INSTANCE)
                  .build();

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

  public static class SpecialRequestBuilder {

    private List<String> pancakes;

    // Test: lines after '({' should be +4
    // Test: final ')}' should be aligned to beginning of the line that opened this block
    @Refactor({
        // Test: this comment should also be +4
        "blah and blah should be required, not optional, and their literal string values should be",
        " removed"
    })
    // Test: when definition arguments are split, remaining lines are +4
    public SpecialRequestBuilder(
        // Test: this comment should also be +4
        String str1, String str1,
        Optional<Special.Context> context1,
        Optional<Special.Context> context2) {

      pancakes = new LinkedList<String>();
    }
  }

  public void voidMethod() {
    // Test: consecutive builder calls are aligned on '.'
    // Note: A comment on any part of the lines after first will cause IntelliJ to format to +4 instead,
    //       which is a bug that we don't emulate
    Response response = webResource.path("/thing1/")
                                   .queryParam("apikey", apiKey)
                                   .queryParam("thing_id", query.getVenueId())
                                   .get(Response.class);
  }
}
