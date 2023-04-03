<?php

namespace App\ServiceLogic;

use App\Repositories\CategoryRepository;
use Illuminate\Support\Str;
use Carbon\Carbon;

/**
 * 
 * @return array
 */

class CategoryService
{

	public function __construct()
	{
		$this->categoryRepository = new CategoryRepository();
	}

	public function findCategorybySlugorFail($slug)
	{
		return $this->categoryRepository->whereFirstorFailWithFormatSorted(['column' => 'slug', 'value' => $slug]);
	}

	public function addCategoriesFromArray($array)
	{

		foreach ($array as $key => $value) {
			
			$url = trim(str::slug($value));

			$this->categoryRepository->create([
				'status' => true,
				'featured' => false,
				'trash' => false,
				'name' => $value,
				'slug' => $url,

			]);
		}

		return 'completed';

	}
}