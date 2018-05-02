app.factory('httpRequestInterceptor', ['$localStorage', function ($localStorage) {
  return {
    request: function (config) {
      if ($localStorage.token) {
          console.log('[httpInterceptor] localStorage.token: ' + $localStorage.token);
          config.headers['Authorization'] = $localStorage.token;
      } else {
        console.log('empty token')
      }
      return config;
    }
  };
}]);

