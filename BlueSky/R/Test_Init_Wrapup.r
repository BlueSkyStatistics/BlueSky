BSkyTest1<-function()
{
	t1 =proc.time()
	BSkyFunctionInit()

	t1 =proc.time()
	print('Function1 calling function2')
	for(i in 1:10) print(i)
	BSkyTest2()
	print(proc.time()-t1)

	t1 =proc.time()
	print('Function1 calling function3')
	for(i in 1:10) print(i)
	BSkyTest2()
	print(proc.time()-t1)

	BSkyFunctionWrapUp()
}

BSkyTest2<-function()
{
	print('Function2 and calling function 4')
	for(i in 1:10) print(i)
	BSkyTest4()
}

BSkyTest3<-function()
{
	BSkyFunctionInit()
	print('Function3 and calling function 4')
	for(i in 1:10) print(i)
	BSkyTest4()
	BSkyFunctionWrapUp()
}

BSkyTest4<-function()
{
	BSkyFunctionInit()
	print('Function4')
	for(i in 1:10) print(i)
	BSkyFunctionWrapUp()
}