var app = angular.module("todoApp", []);

app.config(function($routeProvider, $locationProvider, $httpProvider) {
  $locationProvider.html5Mode(true);
  $routeProvider
    .when("/list/:listName", {controller: "ListCtrl", templateUrl: "/list.html"})
    .otherwise({redirectTo: "/list/inbox"});
  $httpProvider.withCredentials = true;
});

app.factory("listFactory", function($http) {
    return {
      getLists : function() {
        return $http.get("/api/lists");
      },
      getList: function(listName) {
        return $http.get("/api/list/"+listName);
      },
      createItem: function(listName, item) {
        return $http.post("/api/list/"+listName, item);
      },
      editItem: function(listName, item) {
        return $http.put("/api/list/"+listName+"/"+item.id, item);
      },
      removeItem: function(listName, item) {
        return $http({
          method: "DELETE",
          url: "/api/list/"+listName+"/"+item.id
        });
      }
    };
});

app.factory("authFactory", function($http) {
    return {
      login: function(assertion) {
        return $http.post("/auth/login", {assertion: assertion});
      },
      logout: function() {
        return $http.post("/auth/logout");
      }
    };
});

app.controller("ListCtrl", function($scope, $routeParams, $http, listFactory) {
  $scope.loggedIn = $.cookie("email");
  
  $scope.listName = $routeParams.listName;
  $scope.list = [];
  listFactory.getList($scope.listName).success(function(data) {
    $scope.list = data;
  });

  $scope.$on("login", function() {
    $scope.loggedIn = true;
    listFactory.getList($scope.listName).success(function(data) {
      $scope.list = data;
    });
  });
  $scope.$on("logout", function() {
    $scope.list = [];
    $scope.loggedIn = false;
  });


  $scope.addItem = function() {
    if ($scope.newItemBody) {
      newItem = {body: $scope.newItemBody, done:false, id: $scope.list.length};
      $scope.list.push(newItem);
      listFactory.createItem($scope.listName, newItem);
      $scope.newItemBody = "";
    }
  };
  $scope.updateItem = function(item) {
    listFactory.editItem($scope.listName, item);
  };
  $scope.removeItem = function(item) {
    listFactory.removeItem($scope.listName, item);
    // Filter $scope.list to remove item, then map the ids of the remaining
    // items to their indices in the list
    $scope.list = _.map(_.filter($scope.list, function(x) {
      return (x.id !== item.id);
    }), function(x, i) {
      x.id = i;
      return x;
    });
  };
});

app.controller("TabsCtrl", function($scope, $location, listFactory) {
  $scope.loggedIn = $.cookie("email");
  $scope.lists = ["inbox"];
  listFactory.getLists().success(function(data) {
    $scope.lists = data.sort();
  });

  $scope.$on("login", function() {
    $scope.loggedIn = true;
    listFactory.getLists().success(function(data) {
      $scope.lists = data.sort();
    });
  });
  $scope.$on("logout", function() {
    $scope.lists = [];
    $scope.loggedIn = false;
  });

  // Ideally, I'd have bound the focus state of the input to showNewListModal,
  // so that it's focused when it appears, and disappears when focus is lost.
  // Unfortunately, Angular doesn't seem to have an ng-focus directive, so I'm
  // resorting to jQuery. 
  $scope.showNewListModal = false;
  $scope.openNewListModal = function() {
    $scope.showNewListModal = true;
    // Can't focus on hidden element. Timeout gives the view time to update.
    setTimeout(function() {$("#newListInput").focus();}, 20);
  };
  $scope.newList = function() {
    $location.path("/list/"+$scope.newListName);
    $scope.lists = _.uniq($scope.lists.concat($scope.newListName).sort());
    $scope.newListName = "";
    $scope.showNewListModal = false;
  };
});

app.controller("AuthCtrl", function($scope, authFactory, $route) {
  $.cookie.defaults.expires = 7;
  $.cookie.defaults.path = "/";
  $.cookie.raw = true;

  $scope.email = $.cookie("email");
  $scope.loggingIn = false;

  $scope.login = function() {
    $scope.loggingIn = true;
    navigator.id.request();
  };
  $scope.logout = function() {
    navigator.id.logout();
  };

  navigator.id.watch({
    loggedInUser: $.cookie("email"),
    onlogin: function(assertion) {
      authFactory.login(assertion).success(function(response) {
        $.cookie("email", response.email);
        $.cookie("session", response.session);
        $scope.email = response.email;
        $scope.loggingIn = false;
        angular.element("html").scope().$broadcast("login");
      }).error( function() {
        navigator.id.logout();
        $scope.loggingIn = false;
      });
    },
    onlogout: function() {
      authFactory.logout();
      $.removeCookie("session");
      $.removeCookie("email");
      $scope.email = "";
      angular.element("html").scope().$broadcast("logout");
    }
  });
});
