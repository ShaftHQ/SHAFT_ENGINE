package testPackage.SHAFTWizard;

public class TestHelpers {
    public static String registrationForm = """
            data:text/html,<html>
            <form action="https://www.w3schools.com/action_page.php?">
              <div class="container">
                <h1>Register</h1>
                <p>Please fill in this form to create an account.</p>
                <hr>
                        
                <label for="email"><b>Email</b></label>
                <input type="text" placeholder="Enter Email" name="email" id="email" required>
                        
                <label for="psw"><b>Password</b></label>
                <input type="password" placeholder="Enter Password" name="psw" id="psw" required>
                        
                <label for="psw-repeat"><b>Repeat Password</b></label>
                <input type="password" placeholder="Repeat Password" name="psw-repeat" id="psw-repeat" required>
                <hr>                        
                <button type="submit">Register</button>
              </div>
            </form>
            </html>""";
}
