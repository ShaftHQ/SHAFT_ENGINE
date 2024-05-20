package testPackage.legacy;

import com.shaft.validation.Validations;

public class RetryAnalyzerTests {
    private int x =1;
//    @Test
    public void expectedToPass(){
        Validations.assertThat()
                .number(1)
                .isEqualTo(1)
                .perform();
    }

//    @Test(dependsOnMethods = {"expectedToPass"})
    public void expectedToFailOnceThenPass(){
        try {
            Validations.assertThat()
                    .number(2)
                    .isEqualTo(x)
                    .perform();
        }catch (Throwable t){
            x++;
            throw t;
        }
    }
}
