app.controller('ListProductsController', function ($scope, $location, DataProductsFactory, ListProductsFactory, $route) {
    var product = ListProductsFactory.query();
    product.$promise.then(function (result) {
        console.log("ListProductsController");
        DataProductsFactory.init(result);
        $scope.products = result;
        console.log(result);
        $scope.products = DataProductsFactory.getProducts();
    });
});

app.controller('ListLatest6ProductsController', function ($scope, $location, DataProductsFactory, ListLatest6ProductsFactory, $route) {
    var product = ListLatest6ProductsFactory.query();
    product.$promise.then(function (result) {
        console.log("ListLatest6ProductsController");
        DataProductsFactory.init(result);
        $scope.products = result;
        console.log(result);
        $scope.products = DataProductsFactory.getProducts();
    });
});

app.controller('ListMyProductsController', function ($scope, $location, DataProductsFactory, ListMyProductsFactory, $route) {
    var product = ListMyProductsFactory.query();
    product.$promise.then(function (result) {
        console.log("ListMyProductsController");
        DataProductsFactory.init(result);
        $scope.myProducts = result;
        console.log(result);
        $scope.myProducts = DataProductsFactory.getProducts();
    });
});

app.controller('ListHistoryProductsController', function ($scope, $location, DataProductsFactory, ListHistoryProductsFactory, $route) {
    var product = ListHistoryProductsFactory.query();
    product.$promise.then(function (result) {
        console.log("ListMyProductsController");
        DataProductsFactory.init(result);
        $scope.historyProducts = result;
        console.log(result);
        $scope.historyProducts = DataProductsFactory.getProducts();
    });
});

app.controller('ListProductsByCategoryController', function ($scope, $location, DataProductsFactory, ListProductsByCategoryFactory, $route) {
    var product = ListProductsByCategoryFactory.query();
    product.$promise.then(function (result) {
        console.log("ListProductsByCategoryController");
        DataProductsFactory.init(result);
        $scope.products = result;
        console.log(result);
        $scope.products = DataProductsFactory.getProducts();
    });
});

app.controller('ViewProductController', function ($scope, $location, ListProductsByIdFactory, $route, $routeParams) {
    var selectedProduct = ListProductsByIdFactory.query({id: $routeParams.id});
    selectedProduct.$promise.then(function (result) {
        $scope.product = result;
    });
});

app.controller('ViewProductsByCategoryController', function ($scope, $location, ListProductsByCategoryFactory, $route, $routeParams) {
    var selectedProduct = ListProductsByCategoryFactory.query({category: $routeParams.category});
    selectedProduct.$promise.then(function (result) {
        $scope.products = result;
    });
});

app.controller('ViewProductsBySearchController', function ($scope, $location, ListProductsBySearchFactory, $route, $routeParams) {
    var selectedProduct = ListProductsBySearchFactory.query({word: $routeParams.word});
    selectedProduct.$promise.then(function (result) {
        $scope.products = result;
    });
});

app.controller('EditProductsController',
    function ($window,
              $scope,
              $location,
              ListProductsByIdFactory,
              EditProductService,
              $route,
              $routeParams) {
        var selectedProduct = ListProductsByIdFactory.query({id: $routeParams.id});
        selectedProduct.$promise.then(function (result, productId) {
            $scope.EditProductsController = $scope.editProductCtrl || {};
            $scope.EditProductsController.productName = result.productName;
            $scope.EditProductsController.price = result.price;
            $scope.EditProductsController.description = result.description;
            $scope.EditProductsController.category = result.category;
            $scope.EditProductsController.productStatus = result.productStatus;
        });

        $scope.edit = function () {
            console.log('Edit product called');
            $scope.loading = true;
            EditProductService.editProduct($scope, $location,
                $scope.EditProductsController.productName,
                $scope.EditProductsController.price,
                $scope.EditProductsController.description,
                $scope.EditProductsController.category,
                $routeParams.id,
                $scope.EditProductsController.productStatus,
                function (result, productId) {
                    if (result === true) {
                        $window.location.href = '/#/productview/' + productId;
                    } else {
                        $scope.error = 'Edit product failed';
                        $scope.loading = false;
                    }
                });
        };
    });

app.controller('ProductsActionsController',
    function ($scope, $location, ListProductsByIdFactory, DeleteProductsFactory, $route) {
        $scope.hoverIn = function () {
            this.Icon = true;
        };

        $scope.hoverOut = function () {
            this.Icon = false;
        };


        $scope.viewProduct = function (productid) {
            ListProductsByIdFactory.query({id: productid}, null);
        };

        $scope.viewProductByCategory = function (productCategory) {
            ListProductsByCategoryFactory.query({category: productCategory}, null);
        };

        $scope.editProduct = function (productid) {
            var selectedProduct = ListProductsByIdFactory.query({id: productid});
            selectedProduct.$promise.then(function (result) {
                EditProductsFactory.update({id: productid}, result);
            });
        };

        $scope.deleteProduct = function (productid) {
            DeleteProductsFactory.delete_product({id: productid}, null);
            $route.reload();
        };
    });
