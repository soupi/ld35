"use strict";

// module Sound

exports.getSound = function(url) {
    return function() {
	return new Howl({ urls: [url], volume: 0.15 });
    };
};

exports.playSound = function(soundObj) {
    return function() {
        return soundObj.play();
    };
};
