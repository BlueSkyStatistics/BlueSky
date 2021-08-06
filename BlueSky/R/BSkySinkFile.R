BSkyWriteToSink.orig <- function (message, bskyfileptr)
{
    #write(message, bskyfileptr)
}

BSkyWriteToSink <- function ()
{

	z=as.u_char(utf8ToInt("BSkyFormat을 사용하여 개체를 포맷 할 수 없습니다. 지원되지 않는 유형입니다. 지원되는 유형은 다음과 같습니다.\n 배열, 매트릭스 데이터 프레임 및 BSky 반환 구조."))
	
	 write(intToUtf8(z), BSkysinkfptr)
}