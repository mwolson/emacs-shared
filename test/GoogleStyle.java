// This file acts like a unit test of sorts for google-c-style.el:
// If any file contents are changed after C-M-\, then the tests fail.
// Each test case is preceded with a comment.
//
// Use 'C-c C-s' on a line to understand which cc-mode syntactic rule(s) are in effect.
//
// Welcome to the horror show.
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

  private final LoadingCache<Long, Venue>
      // Test outline (can't inline these comments due to IntelliJ bug):
      // Test: wrapping after a type and before a variable name is +4
      // Test: the rest is aligned on '.'
      // Test: inside of argument continuations on lineContinuer will be +4
      // Test: inside of class expression "new CacheLoader" will be +2, even though inside a function call
      // Test: methods inside class expression CacheLoader will be + 2
      // Test: trailing ')}' is also aligned on '.'
      venueCache4 = CacheBuilder.newBuilder()
                                .expireAfterWrite(1, TimeUnit.DAYS)
                                .lineContinuer(
                                    new Foo(),
                                    new Bar())
                                .build(new CacheLoader<Long, Venue>() {
                                  public Venue load(Long venueId) {
                                    return delegate.getVenue(venueId);
                                  }
                                });

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

  protected final Func2<Event, Event, Map<String, Object>> buildModel =
      new Func2<Event, Event, Map<String, Object>>() {
        // Test: this command and @Override line and function decl are all lined up on +2
        @Override
        public ListenableFuture<Map<String, Object>> run(Event event, Event eventevent) {
          final Map<String, Object> map = ImmutableMap.<String, Object>of(
              "name", event.getName(),
              "id", event.getID(),
              "caller", caller);
          return immediateFuture(map);
        }
      };

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

  // Test: 2nd line of function is aligned with first, not indented
  public void addingNoOp1() {
    2
    + 2;
  }

  // Test: 2nd line of function is aligned with first, not indented
  public void addingNoOp2() {
    2 +
    2;
  }

  // Test: 2nd line of function is aligned with '(' +1
  public void addingNoOp3() {
    (2
     + 2);
  }

  // Test: 2nd line of function is aligned with '(' +1
  public void addingNoOp4() {
    (2 +
     2);
  }

  // Test SKIP: 2nd line is indented +4
  // IntelliJ bug: does not place a space between "+" and "2" on 2nd line
  // 2
  //     + 2;

  // Test SKIP: 2nd line is indented +4
  // IntelliJ bug: does not place a space between "2" and "+" on 1st line
  // 2 +
  //     2;

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

      if (pancakeContext.isPresent()) {
        this.pancakeBaker = new PancakeBaker(pancakeContext.get());
      } else {
        // Test: even though we have two open function calls, indent to +4 just once
        this.pancakeBaker = new PancakeBaker(new Special.Context(
            "lalalalalalalala".getBytes(UTF_8),
            ("fkaehfoirenfeolkanfoieogineaolgnleaknmgelainglean gaebaeginaepgojaegnmeapogjeap;g;eoagjpeaijgpaeojgpeoa"
             // Test: align this comment and following line to paren +1
             + "feoifeoijnbio").getBytes(UTF_8),
            ("gfeoijgoeingoeingle;iaogoin eaogneaoigneaoingoeian goiengoieangoieanglkneklap;g[kmn;glknaepopojpoagjpoj"
             + "fgoeijoignoin").getBytes(UTF_8)));
      }

      if (baconContext.isPresent()) {
        this.baconCryptor = new BaconFryer(urlContext.get());
      } else {
        // Test: even though we have two open function calls, indent to +4 just once
        this.baconCryptor = new BaconFryer(new Special.Context(
            "lalalalalalalalalalallalalalal".getBytes(UTF_8),
            "feoijf0o9 3e093jpoif3j pfiojff".getBytes(UTF_8),
            "f3ofjh3jf039jkf039mp3omf093jff".getBytes(UTF_8)));
      }
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
  } // Test: '}' belonging to enclosing method is aligned at left margin +2 after all that

  public PancakeRequest returnNewThing() {
    Optional<String> string;

    if (!stringParams.isEmpty()) {
      String value = "foo";
      string = Optional.of(value);
    } else {
      string = Optional.absent();
    }

    // Test: builder call is +4, next function call continuation line is another +4
    final FeatureWrapper features = new Gson()
        .fromJson(new InputStreamReader(resource.getInputStream(), Charset.defaultCharset()),
            FeatureWrapper.class);

    return new PancakeRequest(
        // Test: comments also +4
        arg1,
        arg2,
        arg3,
        arg4);
  }

  public PancakeRequest returnBuilder() {
    // Test: line continuation of chained builder on a return should be +4
    return PancakeRequest
        // Test: comments also +4
        .build(arg1, arg2, arg3)
        .method(Methods.GET)
        .header("Content-Type", "application/x-www-form-urlencoded")
        .superParam("foo1", "bar1")
        .superParam("foo2", criteria.bar)
        .encryptedQueryParam(
            // Test: function continuation (and comment) is additional +4
            "text",
            criteria.bar + "|" + criteria.quux,
            criteria.baz.toString())
        .moreParam("qubar", criteria.quux + "flim" + criteria.bar, "")
        .moreParam("qubbar", criteria.quux, "")
        .createRequest();
  }

  public HttpServletRequest returnWithContinuation() {
    // Test: deals with return + line continuation, applies +4 to line after
    return ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes())
        .getRequest();
  }

  public String returnWithBuilder1() {
    // Test: deals with return + builder continuation, aligns to '.'
    return MoreObjects.toStringHelper(this)
                      .add("value", value)
                      .toString();
  }

  public String returnWithBuilder2() {
    // IntelliJ bug: a line comment before .toString() gets indented an additional +4
    // Test: deals with return + builder continuation before '.', applies +4 to remaining
    return MoreObjects
        .toStringHelper(this)
        .add("value", value)
        .toString();
  }

  @Bean
  public SpecialEventFilter specialEventFilter1(
      // Test: this line and next (with annotations) are +4
      @Value("${ALLOW_THIS}") final String commaDelimitedThings,
      @Value("${RESTRICT_TO_ONLY_THIS}") final boolean strict) {

    final Set<String> activeEventIds;
    if (commaDelimitedThings != null && !commaDelimitedThings.trim().isEmpty()) {
      activeEventIds = ImmutableSet.copyOf(Arrays.asList(commaDelimitedThings.split(",")));
    } else {
      activeEventIds = Collections.emptySet();
    }

    return new SpecialEventFilterImpl(strict, commaDelimitedThings);
  }

  @Bean
  public SpecialEventFilter specialEventFilter2(
      // Test: this line and next few (with annotations on own lines) are all +4
      @Value("${ALLOW_THIS}")
      final String commaDelimitedThings,
      // Test: The ')' aligns with '@' and body lines are +4
      @ValuePlus(
          "${RESTRICT_TO_ONLY_THIS}",
          "but not this"
      )
      // Test: The next line after the annotation aligns with '@' and body lines are +4
      @ValuePlusPlus(
          "${RESTRICT_TO_ONLY_THIS}",
          "but not this")
      final boolean strict,

      @Value("${BUT_NOT_THIS}") // this line survives the extra whitespace to remain aligned with others
      final boolean lax) {

    final Set<String> activeEventIds;
    if (commaDelimitedThings != null && !commaDelimitedThings.trim().isEmpty()) {
      activeEventIds = ImmutableSet.copyOf(Arrays.asList(commaDelimitedThings.split(",")));
    } else {
      activeEventIds = Collections.emptySet();
    }

    // Test: line break after 1st arg should cause 2nd arg to be +4
    appendLogEntry(accessLogBuilder,
        Optional.fromNullable(httpServletRequest.getHeader("Referer")));

    LOG.info(String.format("Rejected parameters request for event id '%s'",
        parametersRequest.criteria.get(0).eventID)); // Test: this aligns to +4, not the opening '('

    return new SpecialEventFilterImpl(strict, commaDelimitedThings);
  }

  // Test: The '})' aligns with '@' and body lines are +4
  @TechDebt({
      "bla bla bla wall of text;",
      " more text."
  })
  @TechDebt("Not asymptotic enough.")
  @RequestMapping
  public ModelAndView handleRequestInternal(
      final HttpServletRequest httpServletRequest,
      final HttpServletResponse response)
      throws IOException { // Test: aligns "throws" to rest of continuation, which is +4

    final JsonNode jsonParams = objectMapper.readTree(httpServletRequest.getInputStream());
  }

  public ModelAndView handleRequestInternalSomeMore(
      final HttpServletRequest httpServletRequest,
      final HttpServletResponse response)
      throws IOException {}

  final JsonNode andWeAreBack = objectMapper.readTree(httpServletRequest.getInputStream());
  // Test: previous line aligns back to margin +2
}
