app.factory('RegistrationService', function ($http, $localStorage, AuthenticationService) {
   var service = {};

    service.newAccount = function (scope, location, email, phone, password, repeated_password, callback) {
         if (password === repeated_password) {
            $http.post('/api/register', { email: email, phone: phone, password: password })
                  .success(function (response) {
                  console.log('response:' + JSON.stringify(response));
                  if (response.created) {
                       AuthenticationService.Login(email, password, function (result) {
                                   if (result === true) {
                                        location.path('/');
                                   } else {
                                        scope.error = 'Username or password is incorrect';
                                        scope.loading = false;
                                   }
                        });
                  } else {
                    scope.error = 'Error while creating a new account';
                    scope.loading = false;
                  }
            });
         }
    };

    return service;
});