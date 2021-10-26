
data irtggum (DROP=tx1-tx1000 bx1-bx20 th2x1-th2x8 th3x1-th3x8	th4x1-th4x8 th5x1-th5x8
					th6x1-th6x8 th7x1-th7x8 th8x1-th8x8 th9x1-th9x8
					th10x1-th10x8 th11x1-th11x8 th12x1-th12x8 th13x1-th13x8
					th14x1-th14x8 th15x1-th15x8 th16x1-th16x8 th17x1-th17x8
					th18x1-th18x8 th19x1-th19x8 th20x1-th20x8 denom num0-num3 x1-x20);
	array tx tx1-tx1000;
	do i= 1 to 1000;
		tx[i]=rannor(8675309);
	end;
	array bx bx1-bx20; *locations;
	do k=1 to 20;
		bx[k]= -1 + (k-1)*0.1;
	end;
	array x x1-x20; *discriminations;
	do k=1 to 20;
		x[k]=rannor(8675393);
	end;

	array prob p0-p3;

*generating thresholds;
	array thx{20,8} th1x1-th1x8 th2x1-th2x8 th3x1-th3x8	th4x1-th4x8 th5x1-th5x8
					th6x1-th6x8 th7x1-th7x8 th8x1-th8x8 th9x1-th9x8
					th10x1-th10x8 th11x1-th11x8 th12x1-th12x8 th13x1-th13x8
					th14x1-th14x8 th15x1-th15x8 th16x1-th16x8 th17x1-th17x8
					th18x1-th18x8 th19x1-th19x8 th20x1-th20x8;

	do h=1 to 20;
		thx(h,1)=0; 
		thx(h,2)=(-0.25 * ranuni(867531))/2;
		thx(h,3)=-0.5 * ranuni(867531) + thx(h,2); 
		thx(h,4)=thx(h,3)-.9;
		thx(h,5)= -1 * thx(h,4);
		thx(h,6)= -1  * thx(h,3);
		thx(h,7)= -1 * thx(h,2);
		thx(h,8)= -1 * thx(h,1);
	end;

	do i= 1 to 1000;
	theta=tx[i];
	do j=1 to 20;
		a=x[j];
		b=bx[j];

		num0=(CONSTANT('E') ** (a*(0*(theta - b)+(thx[j,1])))) + 
			(CONSTANT('E') ** (a*(7*(theta - b)+(thx[j,1]+thx[j,2]+thx[j,3]+thx[j,4]
			+thx[j,5]+thx[j,6]+thx[j,7]+thx[j,8]))));
		num1=(CONSTANT('E') ** (a*(1*(theta - b)+(thx[j,1]+thx[j,2])))) + 
			(CONSTANT('E') ** (a*(6*(theta - b)+(thx[j,1]+thx[j,2]+thx[j,3]+thx[j,4]
			+thx[j,5]+thx[j,6]+thx[j,7]))));
		num2=(CONSTANT('E') ** (a*(2*(theta - b)+(thx[j,1]+thx[j,2]+thx[j,3])))) + 
			(CONSTANT('E') ** (a*(5*(theta - b)+(thx[j,1]+thx[j,2]+thx[j,3]+thx[j,4]
			+thx[j,5]+thx[j,6]))));
		num3=(CONSTANT('E') ** (a*(3*(theta - b)+(thx[j,1]+thx[j,2]+thx[j,3]+thx[j,4])))) + 
			(CONSTANT('E') ** (a*(4*(theta - b)+(thx[j,1]+thx[j,2]+thx[j,3]+thx[j,4]
			+thx[j,5]))));
		
		denom=num0+num1+num2+num3;
		p0=num0/denom; *probabilities;
		p1=num1/denom;
		p2=num2/denom;
		p3=num3/denom;

		rsp=rand("Table", of prob[*])-1; *minus one because the categories are actually 0-3;

		*adding response time;
		RT = (7500 - (abs(1-abs(theta-b))*.75)) + (1737.12*(-1 + (1-(-1))*ranuni(867531)));

		output;
		end;
	end;
run;

proc transpose data=irtggum out=resp prefix=rsp;
	by i;
	var rsp rt;
run;

data resp;
	set resp;
	drop i _NAME_;
run;
