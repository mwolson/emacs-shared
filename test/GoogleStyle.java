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
      // https://youtrack.jetbrains.com/issue/IDEA-136980
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

  private final LoadingCache<Long, Venue>
      // Test outline (can't inline these comments due to IntelliJ bug):
      // https://youtrack.jetbrains.com/issue/IDEA-136980
      // Test: wrapping after a type and before a variable name is +4
      // Test: the rest is aligned on '.'
      // Test: inside of argument continuations on lineContinuer will be +4
      // Test: inside of class expression "new CacheLoader" will be +2, even though inside a function call
      // Test: methods inside class expression CacheLoader will be + 2
      // Test: trailing ')}' is also aligned on '.'
      venueCache5 =
      CacheBuilder.newBuilder()
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

  // Note: Pasting this into an Emacs java-mode buffer will cause an error, though loading fresh works fine
  protected final Func2<Event, Event, Map<String, Object>> buildModel1 =
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

  // Test SKIP: IntelliJ incorrectly formats "@ValuePlus" +0 instead of +4
  // protected final Func2<Event, Event, Map<String, Object>> buildModel2 = ImmutableMap.<String, Object>of(
  //     @ValuePlus(
  //         "${RESTRICT_TO_ONLY_THIS}",
  //         "but not this"
  //     )
  //     "name",
  //     event.getName(),
  //     "id",
  //     event.getID());

  int product = 1
                // Test: this comment should be aligned 2 char after the '='
                * 2 * 2 // Test: this should be aligned 2 char after the '='
                * 3
                * 4;

  public CachingCrudClient() {}

  public boolean returnMethod1() {
    return 1 == 1
           && (0 != 1) // Test: this should be aligned to end of return statement plus whitespace
           // Test: this comment should align siimilarly
           && 2 != 1;
  }

  public boolean returnMethod2() {
    // Note: this isn't correct Google Style, since it doesn't allow break after operators
    return 1 == 1 &&
           (0 != 1) && // Test: this should be aligned to end of return statement plus whitespace
           // Test: this comment should align siimilarly
           2 != 1;
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

  // Test: trailing ')' is aligned with open paren
  public void addingNoOp5() {
    (2 +
     2
    );
  }

  // Test: 2nd line of function is aligned with first, not indented
  public void addingNoOp5() {
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

    // Test: 2nd line is aligned with first, not indented
    2
    + 2;
  }

  // Test SKIP: 2nd line is indented +4
  // IntelliJ bug: does not place a space between "+" and "2" on 2nd line
  // https://youtrack.jetbrains.com/issue/IDEA-136985
  // 2
  //     + 2;

  // Test SKIP: 2nd line is indented +4
  // IntelliJ bug: does not place a space between "2" and "+" on 1st line
  // https://youtrack.jetbrains.com/issue/IDEA-136985
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
    // https://youtrack.jetbrains.com/issue/IDEA-122802
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
      final boolean strict,
      // Test: The next line after the annotation aligns with '@' and body lines are +4
      @ValuePlusPlus(
          "${RESTRICT_TO_ONLY_THIS}",
          "but not this")
      final boolean stricter,

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

  public void ifCondition1() {
    final Observable<Event> modelMerge =
        Observable.zip(details1, details2, new Func2<Event, Event, Event>() {
          @Override
          public Event call(Event details1, Event details2) {

            if (eventInfo.getDetails() != null) {
              eventInfo.setDetails(eventInfo.getDetails());
            } else {
              LOG.debug("Not setting formatted date for event ID " + eventInfo.getId());
            }

            if (EventPredicates.hasDetails(details)) {
              eventInfo.setErrorMessage(GENERIC_ERROR_MESSAGE);
            } else if (EventPredicates.detailsEmpty(eventInfo)) {
              eventInfo.setErrorMessage(UNKNOWN_DETAILS_ERROR_MESSAGE);
              // Test: if conditions should be wrapped after '('
            } else if (eventInfo.getDetails().getFormat() != null &&
                       // Test: comment should be wrapped after '(' as well
                       eventInfo.getDetails().getFormat().equals("text")) {
              // Test: next line should be aligned with leading 'if' statement, not the one 4 lines up
              eventInfo.setErrorMessage(UNKNOWN_DETAILS_ERROR_MESSAGE);
              // Test: next line should be aligned with leading 'if' statement, not the one 4 lines up
            } else if (EventPredicates.detailsAfterBlah(eventInfo)) {
              if (EventPredicates.hasDetails(eventInfo)) {
                String date = formatDate(eventInfo);
                eventInfo.setErrorMessage(DETAILS_ERROR_MESSAGE);
                eventInfo.setErrorMessageSubtext(
                    String.format(DETAILS_ERROR_MESSAGE_SUBTEXT, date));
              } else {
                eventInfo.setErrorMessage(GENERIC_ERROR_MESSAGE);
              }
            }
            return eventInfo;
          }
        });
  }

  // Test: The '})' aligns with '@' and body lines are +4
  // Note: Google Style allows these to be +2, but IntelliJ does not, per this bug report:
  // https://youtrack.jetbrains.com/issue/IDEA-54564
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

  // Test: IntelliJ should leave the collapsed block alone
  public ModelAndView handleRequestInternalEmpty() {}

  // Test SKIP: IntelliJ fails to leave the collapsed block collapsed, if there's at least one line break before it
  // https://youtrack.jetbrains.com/issue/IDEA-136990
  // public ModelAndView handleRequestInternalEmptyMulti(
  //     final HttpServletRequest httpServletRequest,
  //     final HttpServletResponse response)
  //     throws IOException {}

  final JsonNode andWeAreBack = objectMapper.readTree(httpServletRequest.getInputStream());
  // Test: previous line aligns back to margin +2

  public class ConcatenateTest {

    // Test: "+" must line up with double-quote on previous line
    private static final String concatenated =
        "YagbooScTxxN3BMFJW7Oqm3ps2P29tihNwnK-UUAGNSEQAfvbNmg1K_cM2kdzInyGlsHWxtvfJT57tYuwkDgsrry6G_9"
        // Test: this comment should also line up with double-quote on previous line
        + "inveJUS_vP-7tMQbLIMh21IuB_WE";
  }

  public class TernaryTest {

    // Test: "?" should be aligned after '='
    String ternary1 = StringUtils.isBlank(configRunLocationOther)
                      // Test: comment should be lined up as well
                      ? configRunLocationLocal : configRunLocationOther;

    // Test: ":" should be indented after '='
    String ternary2 = StringUtils.isBlank(configRunLocationOther)
                      // Test: comment should be lined up as well
                      ? configRunLocationLocal
                      : configRunLocationOther;

    // Test: "?" should be lined up with previous line (not an additional +4)
    String ternary3 =
        StringUtils.isBlank(configRunLocationOther)
        ? configRunLocationLocal : configRunLocationOther;

    // Test SKIP: ":" should be lined up with previous line (not an additional +4, not lined up with "?")
    // IntelliJ bug: always aligns with "?", can't turn it off
    // https://youtrack.jetbrains.com/issue/IDEA-136986
    // String ternary4 = StringUtils.isBlank(configRunLocationOther) ? configRunLocationLocal
    //                   : configRunLocationOther;

    // Test: "?" and ":" lines should each be aligned with previous line (not an additional +4)
    String ternary5 =
        StringUtils.isBlank(configRunLocationOther)
        ? configRunLocationLocal
        : configRunLocationOther;
  }

  public class AnnotationsTest {
    // Test FAIL: Should not indent the "public class" line
    // @Options(
    //     fail = false)
    // public class RunCucumber {}

    // Test FAIL: Should not indent the 2nd "@TechDebt" line
    // @TechDebt({
    //     "line 1;",
    //     " line 2."})
    // @TechDebt("line 3.")
  }

  public class ArrayInitializerTest {

    // IntelliJ bug: this should be +2 according to Google, but gets formatted +4, with no way to change it
    // https://youtrack.jetbrains.com/issue/IDEA-54564
    final String[] style1 = {
        // Test: for consistency with IntelliJ bug, should be +4
        1,
        2,
        3
    };

    // Test: should be kept on one line
    final String[] style2 = {1, 2, 3};

    final String[] style3 = ({
        // Test: for consistency with IntelliJ bug, should be +4
        1, 2, 3
    });

    final String[] style4 = ({
        // Test: for consistency with IntelliJ bug, should be +4
        1, 2, 3
        // IntelliJ bug: comments after '}' but before ')' should be aligned to the ')', not +4
        // https://youtrack.jetbrains.com/issue/IDEA-136988
    }
    ); // Test: should be aligned with beginning of "final"

    public void sideEffects1() {
      // Test FAIL: Contents should be lined up +4
      // new int[]{
      //     1,
      //     2,
      //     3
      // }.forEach(print);
    }

    public void sideEffects2() {
      // Test FAIL: Contents should be lined up +4
      // (new int[]{
      //     1,
      //     2,
      //     3
      // }).forEach(print);
    }
  }

  public class LambdaTest {

    final ListenableFuture<DigitalData> digitalDataModel = modelMerge.map(e -> {
      // Test: This comment and block contents should be indented +2
      if (!e.getServiceFailure()) {
        EventAnalyticsPostProcessor analytics = new EventAnalyticsPostProcessor();
        return analytics.postProcess(e);
      } else {
        return null;
      }
    });

    public FeatureBundle getFeatures(FeaturesContext request) {
      final Map<String, String> overrides = request.getOverrides();
      final ImmutableMap.Builder<String, Object> featureMapBuilder = ImmutableMap.builder();

      // Test: lambda blocks that are not part of an assignment get indented +2
      features.forEach(feature -> {
        // This comment is also +2
        String override = overrides.get(feature.key());
        if (override != null) {
          FeatureValueHydrator valMaker = flagHydrators.get(feature.key());
          featureMapBuilder.put(feature.key(), valMaker.value(override));
        } else {
          featureMapBuilder.put(feature.key(), feature.value(request));
        }
      });

      // Test: open lambda blocks as function params are +4
      // Test: end of open lambda blocks are aligned with the first argument
      final ListenableFuture<Something> somethingModel = somethingMerge.map(
          e -> {
            if (!e.problemo()) {
              SomethingPostProcessor something = new SomethingPostProcessorPostProcessor();
              return something.postProcess(e);
            } else {
              return null;
            }
          });

      return new FeatureBundle(featureMapBuilder.build());
    }

    // Test: lambda blocks that are part of a chained builder call get indented +2 relative to '.'
    public void chainedLambdas() {
      Collections.list(request.getHeaderNames())
                 .stream()
                 .filter(header -> header.startsWith(FEATURE_HTTP_HEADER_NAME))
                 .forEach(header -> {
                   Iterable<String> parts = FEATURE_VALUE_SPLITTER.split(header);
                   String value = request.getHeader(header);
                   if (Iterables.size(parts) == 2) {
                     this.addOverride(Iterables.get(parts, 1), value);
                   } else {
                     LOG.warn("unable to override {} with value {}", header, value);
                   }
                 });
    }
  }
}
