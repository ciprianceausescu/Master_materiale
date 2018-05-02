app.controller('LoginController', function($scope , $location, AuthenticationService, $localStorage, $route){
    $scope.login = function() {
        console.log('Login called');
        $scope.loading = true;
        AuthenticationService.Login($scope.email, $scope.password, function (result) {
            if (result === true) {
                $location.path('/#');

            } else {
                $scope.error = 'Username or password is incorrect';
                $scope.loading = false;
            }
        });
    };

    initController();

    function initController() {
        // reset login status
        AuthenticationService.Logout();
    }

    $scope.logout = function logout() {
        AuthenticationService.Logout();
        $route.reload();
        console.log('AAAA' + $localStorage.token);
    };

    $scope.hasLoggedUser = function() {

        if ($localStorage.token && $localStorage.token != '') {
            return true;
        } else {
            return false;
        }

    }
});