app.factory('AuthenticationService', function ($http, $localStorage) {
   var service = {};

    service.Login = function (email, password, callback) {
         $http.post('/api/authenticate', { email: email, password: password })
              .success(function (response) {
              console.log('response:' + JSON.stringify(response));
               // login successful if there's a token in the response
               if (response.token) {
               // store token in local storage to keep user logged in between page refreshes
                   $localStorage.token = response.token;
                    // set up interceptor for all requests
                    callback(true);

               } else {
                    // execute callback with false to indicate failed login
                    callback(false);
               }
               });

    };

    service.Logout = function () {
        // remove user from local storage and clear http auth header
        $localStorage.token = '';
    };

    service.hasLoggedUser = function(callbackFn) {
        if ($localStorage.token && $localStorage.token !== '') {
            callbackFn(true);
        } else {
            callbackFn(false);
        }

    };

    return service;
});
