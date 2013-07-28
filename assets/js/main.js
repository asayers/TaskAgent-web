var app = angular.module("todoApp",[]);

app.config(function($routeProvider, $locationProvider) {
  $locationProvider.html5Mode(true);
  $routeProvider
    .when("/list/:listName", {controller: "ListCtrl", templateUrl: "/list.html"})
    .otherwise({redirectTo: "/list/inbox"});
});

app.factory("listFactory", function($http) {
    return {
      getLists : function() {
        return $http.get("/api/");
      },
      getList: function(listName) {
        return $http.get("/api/"+listName);
      },
      createItem: function(listName, item) {
        return $http.post("/api/"+listName, item);
      },
      editItem: function(listName, item) {
        return $http.put("/api/"+listName+"/"+item.id, item);
      },
      removeItem: function(listName, item) {
        return $http.delete("/api/"+listName+"/"+item.id);
      }
    };
});

app.controller("ListCtrl", function($scope, $routeParams, $http, listFactory) {
  $scope.listName = $routeParams.listName;
  $scope.list = [];
  listFactory.getList($scope.listName).success(function(data) {
    $scope.list = data;
  });

  $scope.addItem = function() {
    newItem = {body: $scope.newItemBody, done:false, id: $scope.list.length};
    $scope.list.push(newItem);
    listFactory.createItem($scope.listName, newItem);
    $scope.newItemBody = "";
  };
  $scope.updateItem = function(item) {
    listFactory.editItem($scope.listName, item);
  };
  $scope.removeItem = function(item) {
    listFactory.removeItem($scope.listName, item);
    $scope.list = deleteAt($scope.list, item.id); // TODO: this is giving some weird behaviour. Try filtering by item.id instead.
  };
});

app.controller("TabsCtrl", function($scope, $location, listFactory) {
  $scope.lists = [];
  listFactory.getLists().success(function(data) {
    $scope.lists = data.sort();
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

function deleteAt (xs, n) {
  ys = xs.splice(n);
  ys.shift();
  return xs.concat(ys);
}
