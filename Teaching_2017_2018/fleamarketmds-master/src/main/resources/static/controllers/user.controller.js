app.controller('UserDetailsController', function ($scope, $http, $location, ListUsersByIdFactory, $route, $routeParams) {
    $http.get("/api/whoami")
        .success(function (result) {
            if (result.loggedin !== -1) {
                console.log(result.loggedin);
                var selectedUser = ListUsersByIdFactory.query({id: result.loggedin});
                console.log(selectedUser);
                selectedUser.$promise.then(function (data) {
                    $scope.user = data;
                });
            }
        });
});