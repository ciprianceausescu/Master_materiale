<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="SQL Injection demo">
    <meta name="author" content="Francesco Borzì">

    <title>SQL Injection Demo</title>

    <link href="css/bootstrap.min.css" rel="stylesheet">
    <link href="css/style.css" rel="stylesheet">
  </head>

  <body>

    <div class="container">
      <div class="header hidden-xs">
        <ul class="nav nav-pills pull-right">
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">Standard Login<b class="caret"></b></a>
            <ul class="nav dropdown-menu">
              <li><a href="login1.php">Vulnerable</a></li>
              <li><a href="login2.php">Secure</a></li>
            </ul>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">Numeric Login<b class="caret"></b></a>
            <ul class="nav dropdown-menu">
              <li><a href="login3.php">Vulnerable</a></li>
              <li><a href="login4.php">Secure</a></li>
            </ul>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">Search<b class="caret"></b></a>
            <ul class="nav dropdown-menu">
              <li><a href="books1.php">Vulnerable</a></li>
              <li><a href="books2.php">Secure</a></li>
            </ul>
          </li>
        </ul>
        <h3 class="text-muted"><a href="index.php">SQL Injection Demo</a></h3>
      </div>
      <?php include("mobile-navbar.php"); ?>

      <div class="jumbotron">
        <img src="images/fmi.png">
        <h2>Database Security</h2>
        <h3>SQL Injection</h3>
      </div>

      <?php include("footer.php"); ?>

    </div> <!-- /container -->

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
    <script src="js/bootstrap.min.js"></script>
  </body>
</html>
