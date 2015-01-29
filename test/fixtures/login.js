(function($, _aoWin) {

	_aoWin.QRLogin = {};
    _aoWin.LoginLog = "";
	var _sBaseHost = "",
        _oLoginQrCodeImg = document.getElementById("loginQrCode");
	if (document.domain == "qq.com") {
		_sBaseHost = "weixin.qq.com";
	} else if(location.hostname.match(/(wechat\.com)$/)){
		_sBaseHost = "wechat.com";
	}else{
        _sBaseHost = "wechatapp.com";
    }

	var show_tip = 1,
		_sCurUUId,
		_oResetTimeout,
        _aWebMMCallbacks = [],
        _oDetactWebMMInterval = setInterval(function(){
            if(_aoWin.WebMM){
                clearInterval(_oDetactWebMMInterval);
                var callback;
                while(callback = _aWebMMCallbacks.shift()){
                    if(typeof(callback) != "function") continue;
                    callback();
                }
            }
        }, 1000);

    function _logInPage(_asLog){
        _aoWin.LoginLog = LoginLog + _asLog + "\n";
    }

    function _afterLoadWebMMDo(callback){
        if(!_aoWin.WebMM){
            _aWebMMCallbacks.push(callback);
        }else{
            callback();
        }
    }

    function _reportNow(text){
        _logInPage(text);
        _afterLoadWebMMDo(function(){
            WebMM.ossLog({Text: text});
            WebMM.flushOssLog();
        });
    }

    var reLoadQRImgCount = 0,
        loadQRCodeTime = 0,
        loadQRImgSucc = function(){
            clearInterval(loadQRImgWatchDog);
            _logInPage("Load QRCode Success, time=" + (new Date().getTime() - loadQRCodeTime) + "ms, reload count: " + reLoadQRImgCount);
        },
        loadQRImgFail = function(img){
            _reportNow("Load QRcode fail!" + status + ", src: " + img.src + ", time: " + (new Date().getTime() - loadQRCodeTime) + "ms");
        },
        loadQRImgWatchDog = null;
	function _loadQRImg(uuid) {
        _poll(uuid);
        _logInPage("Load QRCode Start");
        loadQRCodeTime = new Date().getTime();

        _oLoginQrCodeImg.onload = function(){
            loadQRImgSucc();
            _oLoginQrCodeImg.onload = null;
        };
        _oLoginQrCodeImg.onerror = function(){loadQRImgFail(this)};
        _oLoginQrCodeImg.src = "https://login."+_sBaseHost+"/qrcode/"+uuid+"?t=webwx";

        loadQRImgWatchDog = setInterval(function(){
            if (reLoadQRImgCount >= 5) {
                _reset();
                return;
            }
            reLoadQRImgCount++;

            var _img = new Image();
            _img.onload = function () {
                if(!_oLoginQrCodeImg.onload) return;

                _oLoginQrCodeImg.onload = null;
                _oLoginQrCodeImg.src = this.src;//replace
                loadQRImgSucc();
            };
            _img.onerror = function(){loadQRImgFail(this)};
            _img.src = _oLoginQrCodeImg.src + "&r=" + new Date().getTime();
        }, 5000);
    }

    var _sSecondRequestTime = 0,
        _nAjaxTimeout = 100 * 1000,
        _nNewLoginFuncErrCount = 0;
	function _poll(_asUUID) {
		var _self = arguments.callee,
            _nTime = 0;
		_sCurUUId = _asUUID;

        _logInPage("_poll Request Start, time: " + new Date().getTime());
        _nTime = new Date().getTime();
		$.ajax({
		type: "GET",
		url: "https://login." + _sBaseHost + "/cgi-bin/mmwebwx-bin/login?uuid=" + _asUUID + "&tip=" + show_tip,
		dataType: "script",
		cache: false,
		timeout: _nAjaxTimeout,
		success: function(data, textStatus, jqXHR) {
            _logInPage("_poll Request Success, code: " + window.code + ", time: " + (new Date().getTime() - _nTime) + "ms");
			switch (_aoWin.code) {
			case 200:
                _sSecondRequestTime = new Date().getTime() - _sSecondRequestTime;
                _logInPage("Second Request Success, time: " + _sSecondRequestTime + "ms");
				clearTimeout(_oResetTimeout);

                var _fNewLoginFunc = function(){
                    $.ajax({
                        url: _aoWin.redirect_uri + "&fun=new",//new login page
                        type: "GET",
                        success:function(msg) {
                            _logInPage("new func reponse, reponseMsg: " + msg);
                            var code = msg.match(/<script>(.*)<\/script>/);
                            var skey=msg.match(/<skey>(.*)<\/skey>/);
                            var wxsid=msg.match(/<wxsid>(.*)<\/wxsid>/);
                            var wxuin=msg.match(/<wxuin>(.*)<\/wxuin>/);
                            var passticket=msg.match(/<pass_ticket>(.*)<\/pass_ticket>/);
                            if(skey && skey[1]){
                                WebMM.model("account").setSkey(skey[1]);
                            }
                            if(wxsid && wxsid[1]){
                                WebMM.wxsid = wxsid[1];
                            }
                            if(wxuin && wxuin[1]){
                                WebMM.wxuin = wxuin[1];
                            }
                            WebMM.passticket = (passticket && passticket[1]) ? passticket[1] : "";

                            if(code){
                                eval(code[1]);
                            }else{
                                $("#container").show();
                                $("#login_container").hide();
                            }
                        },
                        error:function(jqXHR, textStatus, errorThrown){
                            _nNewLoginFuncErrCount++;
                            if(_nNewLoginFuncErrCount > 5){
                                if(confirm("Call new login page func error, refresh?")){location.reload()}
                                return;
                            }
                            _reportNow(_aoWin.redirect_uri + " New login page func error: " + textStatus +" retryCount:" + _nNewLoginFuncErrCount);
                            setTimeout(_fNewLoginFunc, 500);
                        }
                    });
                };
                _fNewLoginFunc();

                _reportNow("/cgi-bin/mmwebwx-bin/login, Second Request Success, uuid: " + _asUUID + ", time: " + _sSecondRequestTime + "ms");
				break;

			case 201:
                clearTimeout(_oResetTimeout);
				show_tip = 0;
				$('.errorMsg').hide();
				$('.normlDesc').hide();
				$('.successMsg').show();
                _reportNow("/cgi-bin/mmwebwx-bin/login, First Request Success, uuid: " + _asUUID);
                _reportNow("/cgi-bin/mmwebwx-bin/login, Second Request Start, uuid: " + _asUUID);

                _sSecondRequestTime = new Date().getTime();

                //_nAjaxTimeout = 5 * 1000;
                _self(_asUUID);
                break;

			case 408:
				setTimeout(function(){
					_self(_asUUID);
				}, 500);
				break;

			case 400:
			case 500:
                _reset();
                _afterLoadWebMMDo(function(){
					_aoWin.Log.d("500, Login Poll Svr Exception");
				});
				break;
			}
		},
		error: function(jqXHR, textStatus, errorThrown) {
			if (textStatus == 'timeout') {
                setTimeout(function(){
                    _self(_asUUID);
                }, 500);
			} else {
                setTimeout(function(){
                    _self(_asUUID);
                }, 5000);

                _logInPage("_poll Request Error:" + textStatus);
                _afterLoadWebMMDo(function(){
                    _aoWin.Log.e("Login Poll Error:" + textStatus);
                });
			}
		}
		});
	}

    var getUUIDCount = 0,
        _getUUIDWatchDog,
        _bGetUUIDSuccess = false;//��ֹajax��successִ�ж��
	function _getUUID() {
        getUUIDCount++;
        var _self = arguments.callee,
            _loadError = function(errorText){
                _reportNow("Load UUID Error! ErrorText: " + errorText + " getUUIDCount=" + getUUIDCount);
                if(getUUIDCount > 5){
                    if (confirm("Load uuid error. Refresh?")) {
                        location.reload();
                    }
                }
                setTimeout(function(){
                    _self();
                }, 500);
            };

        clearTimeout(_getUUIDWatchDog);
        _getUUIDWatchDog = setTimeout(function(){
            if(!_aoWin.QRLogin.code){
                _logInPage("GetUUID Timeout, WatchDog Run");
                _self();
            }
        }, 10000);

        $.ajax({
            type: "GET",
            url: "https://login." + _sBaseHost + "/jslogin?appid=wx782c26e4c19acffb&redirect_uri="+encodeURIComponent(location.protocol+"//"+location.host+"/cgi-bin/mmwebwx-bin/webwxnewloginpage")+"&fun=new&lang=" + document.lang,
            dataType: "script",
            cache: false,
            success : function(){
                clearTimeout(_getUUIDWatchDog);
                if(_bGetUUIDSuccess) return;
                if (_aoWin.QRLogin && _aoWin.QRLogin.code == 200) {
                    _logInPage("GetUUID Success, UUID=" + QRLogin.uuid);
                    _bGetUUIDSuccess = true;

                    clearTimeout(_oResetTimeout);
                    _oResetTimeout = setTimeout(function(){
                        location.reload();//Note: Don't run _reset(). If you run _reset(), there will may have many _poll request, as they get 408 return code
                    }, 5 * 60 *1000);//5 mins

                    _loadQRImg(QRLogin.uuid);
                } else {
                    var QRLoginCode = (_aoWin.QRLogin && _aoWin.QRLogin.code) ? _aoWin.QRLogin.code : "None";
                    _logInPage("GetUUID Error, QRLogin.code=" + QRLoginCode);
                    _loadError("QRLogin.code= "  + QRLoginCode);
                }
            },
            error : function(xhr, textStatus, errorThrown){
                _logInPage("GetUUID Error, textStatus=" + textStatus);
                _loadError(textStatus);
            }
        });
	}

    function _reset(){
        location.reload();
    }

	if ($("#login_container").is(":visible") ) {
        _getUUID();
	}


	var _bHadLog = false;
	function _ossLog() {
		if (_bHadLog) return;
		_bHadLog = true;
		var _sUvid = document.cookie.match(new RegExp( "(^| )"+"webwxuvid"+"=([^;]*)(;|$)"));
        if(!_sUvid || _sUvid.length < 3) return;
        _sUvid = _sUvid[2];
		(new Image()).src = "/cgi-bin/mmwebwx-bin/webwxstatreport?funkey=indexdemo&uvid="+_sUvid+"&uuid="+_sCurUUId;
	}


	if($("img.guide").length > 0) {
		var _nTimer = 0,
			_oGuide$ = $(".guide"),
			_oGuideTrigger$ = $("#guideTrigger, #tipTrigger"),
			_oMask$ = $(".mask");

			function _back() {
				_nTimer = setTimeout(function() {
				_oMask$.stop().animate({opacity:0}, function(){$(".mask").hide()});
				_oGuide$.stop().animate({marginLeft:"-120px",opacity:0}, "400", "swing",function(){
					_oGuide$.hide();
				});
			}, 100);
		}

		/*guide*/
		_oGuide$.css({"left":"50%", "opacity":0});
		_oGuideTrigger$.css({"backgroundColor":"white", "opacity":"0"});
		_oGuideTrigger$.mouseover(function(){
			clearTimeout(_nTimer);
			_oMask$.show().stop().animate({"opacity":0.2});
			_oGuide$.css("display", "block").stop().animate({marginLeft:"+168px", opacity:1}, 900, "swing", function() {
				_oGuide$.animate({marginLeft:"+153px"}, 300);
			});
			_ossLog();
		}).mouseout(_back);

		_oGuide$.mouseover(function(){
			clearTimeout(_nTimer);
		}).mouseout(_back);
	}
})(jQuery, window);
