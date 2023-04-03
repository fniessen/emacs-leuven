<?php

namespace App\Http\Controllers\Frontend;

use App\Http\Controllers\Controller;
use Illuminate\Http\Request;
use App\ServiceLogic\CategoryService;
use App\ServiceLogic\FormatService;

class CategoryController extends Controller
{
	public function __construct()
	{
		$this->categoryService = new CategoryService();
		$this->formatService = new FormatService();
	}

	public function get(Request $request)
	{
		$cat = $this->categoryService->findCategorybySlugorFail($request->slug);

		return view('pages.category.single', compact('cat'));
	}
}