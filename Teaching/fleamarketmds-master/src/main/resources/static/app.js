var app = angular.module('FleaMarketApp', ['ngRoute', 'ngResource', 'ui.bootstrap', 'ngStorage']);

app.config(function ($routeProvider, $httpProvider) {
    $routeProvider
        .when('/',
            {
                controller: 'ListLatest6ProductsController',
                templateUrl: '/views/homepage.html'
            })
        .when('/product',
            {
                controller: 'ListProductsController',
                templateUrl: '/views/products.html'
            })
        .when('/product/new',
            {
                controller: 'AddProductController',
                templateUrl: '/views/addProduct.html'
            })
        .when('/profile/myProducts',
            {
                 controller: 'ListMyProductsController',
                templateUrl: '/views/userpage.html'
            })
        .when('/profile/history',
            {
                controller: 'ListHistoryProductsController',
                templateUrl: '/views/historypage.html'
            })
        .when('/login',
            {
                controller: 'LoginController',
                templateUrl: '/views/loginpage.html'
            })
        .when('/register',
            {
                controller: 'RegisterController',
                templateUrl: '/views/registerpage.html'
            })
        .when('/productlist/:category',
            {
                controller: 'ViewProductsByCategoryController',
                templateUrl: '/views/productlist.html'
            })
        .when('/search/:word',
            {
                controller: 'ViewProductsBySearchController',
                templateUrl: '/views/productlistSearch.html'
            })
        .when('/productview/:id',
            {
                controller: 'ViewProductController',
                templateUrl: '/views/productview.html'
            })

        .when('/editProduct/:id',
            {
                controller: 'EditProductsController',
                templateUrl: '/views/editProduct.html'
            })
        .otherwise({
            redirectTo: '/'
        })

        $httpProvider.interceptors.push('httpRequestInterceptor');

});

