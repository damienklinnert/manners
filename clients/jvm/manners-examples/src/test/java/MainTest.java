import io.manners.DSL;
import io.manners.FakeProvider;
import io.manners.ProviderSpecification;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class MainTest {

    private static final FakeProvider fp = new FakeProvider("java-example", "imaginary-service");

    @BeforeClass
    public static void setUp() {
        System.out.println("@Before setUp");
        fp.start();
    }

    @AfterClass
    public static void tearDown() {
        System.out.println("@After tearDown");
        fp.stop();
    }

    @After
    public void reset() {
        fp.verify();
    }

    @Test
    public void doThings() throws Exception {

        final ProviderSpecification spec = DSL.specification()
                .statefulInteraction(
                        "test",
                        DSL.state("state"),
                        DSL.onRequest("get", "/test").queryParam("a", "23"),
                        DSL.respondWith(202)
                );

        fp.setup(spec);
        Main.performRequest();
    }

}
