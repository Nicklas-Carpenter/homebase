fn is_even(n)
	if n == 1 then
		return 0;
	else
		print 0;
	end

	return is_odd(n - 1);
end

fn is_odd(n)
	if n == 1 then
		return 1;
	else
		print 1; 
	end

	return is_even(n - 1);
end

return is_even(57);
