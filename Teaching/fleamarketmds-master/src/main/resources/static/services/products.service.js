app.factory('AddProductsFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products', {}, {
        save: { method: 'POST' }
    });
});

app.factory('DeleteProductsFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/:id',{}, {
        delete_product: { method: 'DELETE' }
    });
});

app.factory('ListProductsFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products', {}, {
        query: { method: 'GET', isArray: true }
    })
});

app.factory('ListLatest6ProductsFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/latest6Products', {}, {
        query: { method: 'GET', isArray: true }
    })
});

app.factory('ListMyProductsFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/my', {}, {
        query: { method: 'GET', isArray: true }
    })
});

app.factory('ListHistoryProductsFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/history', {}, {
        query: { method: 'GET', isArray: true }
    })
});

app.factory('ListProductsByIdFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/:id',{}, {
        query: { method: 'GET' }
    });
});

app.factory('ListProductsByCategoryFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/category/:category',{}, {
        query: { method: 'GET', isArray: true  }
    });
});

app.factory('ListProductsBySearchFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/search/:word',{}, {
        query: { method: 'GET', isArray: true  }
    });
});

app.factory('DataProductsFactory', function(){
    var ProductsList = [];

    var init = function(productsList){
        ProductsList = angular.copy(productsList);
    };

    var addProduct = function(newproduct) {
        ProductsList.push(newproduct);
    };

    var getProducts = function(){
        return ProductsList;
    };

    var getProductById = function (id) {
    }
    return {
        init : init,
        addProduct: addProduct,
        getProducts: getProducts
    };
});

app.factory('AddProductService', function ($http, $localStorage) {
    var service = {};

    service.newProduct = function (scope, location, product_name, price, description, category,imageUrl, callback) {
        $http.post('/products', { productName: product_name, price: price, description: description,
            category:category, imageUrl: imageUrl, publishDate: null, productStatus:"ENABLED"})
            .success(function (response) {
                    console.log('response:' + JSON.stringify(response));
                    if (response.productId) {
                        callback(true, response.productId);
                    }
                }
            );

    };
    return service;
});

app.factory('EditProductsFactory', function ($resource, $location) {
    return $resource(($location.protocol() + "://" + $location.host() + ":" + $location.port()) + '/products/:id',{}, {
        update: { method: 'PUT' }
    });
});

app.factory('EditProductService', function ($http, $localStorage) {
    var service = {};
    service.editProduct = function (scope, location, product_name, price, description, category, id, productStatus, callback) {
        $http.put('/products/' + id, {productName: product_name, price: price, description: description,
            category: category, publishDate: null, productStatus: productStatus})
            .success(function(response) {
                    console.log('response:' + JSON.stringify(response));
                    if (response.productId) {
                        callback(true, response.productId);
                    }
                }
            );
    };
    return service;
});

app.directive('fileModel', ['$parse', function ($parse) {
    return {
        restrict: 'A',
        link: function(scope, element, attrs) {
            var model = $parse(attrs.fileModel);
            var modelSetter = model.assign;

            element.bind('change', function(){
                scope.$apply(function(){
                    modelSetter(scope, element[0].files[0]);
                });
            });
        }
    };
}]);