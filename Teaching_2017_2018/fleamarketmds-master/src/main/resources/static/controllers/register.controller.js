app.controller('RegisterController', function($window, $scope , $location, RegistrationService, AuthenticationService){
    $scope.register = function() {
        console.log('Register called');
        $scope.loading = true;
        RegistrationService.newAccount($scope, $location, $scope.email_new, $scope.phone,
            $scope.password_new, $scope.repeat_password,
            function (result) {
                if (result === true) {
                    $window.location.href='/';
                } else {
                    $scope.error = 'Registration failed';
                    $scope.loading = false;
                }
            });
    };

    initController();

    function initController() {
        // reset login status
        AuthenticationService.Logout();
    }
});