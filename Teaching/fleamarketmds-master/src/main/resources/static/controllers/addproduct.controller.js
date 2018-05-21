app.controller('AddProductController', function($window, $scope , $location, AddProductService, $http) {
    $scope.submitUser = function() {
        alert("intra");

    };

    $scope.addProduct = function () {
        console.log('Add product called');
        var file = $scope.myFile;
        var uploadUrl = "products/upload";
        var fd = new FormData();
        fd.append('file', file);
        fd.append('user',angular.toJson($scope.user,true));
        $http.post(uploadUrl, fd, {
            transformRequest : angular.identity,
            headers : {
                'Content-Type' : undefined
            }
        }).success(function(data) {
            console.log('success');
            $scope.imageUrl = data.imageUrl;
            AddProductService.newProduct($scope, $location, $scope.productName, $scope.price,
                $scope.description, $scope.category,$scope.imageUrl,

                function (result) {
                    if (result === true) {
                        $location.path("/profile/myProducts");
                    } else {
                        $scope.error = 'Add product failed';
                        $scope.loading = false;
                    }
                });
        }).error(function() {
            console.log('error');
        });
    }
});